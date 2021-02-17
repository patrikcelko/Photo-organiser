import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.Char
import Data.Time.Format
import Data.Time.Clock
import Data.List
import Control.Exception.Base
import Text.Read
import System.Environment
import System.IO.Error
import Data.Maybe
import System.Console.GetOpt

data ProgramMode = HelpMode | CleanMode | DryRunMode | BasicMode | ErrorMode | PathErrorMode
    deriving(Eq)

data PhotoData = Photo { getInPath :: FilePath, getPhotoTime :: UTCTime, getOutPath :: FilePath, isInFile :: Bool }
    deriving(Eq)

instance Ord PhotoData where
    (<=) photoA photoB = getPhotoTime photoA <= getPhotoTime photoB
    
instance Show PhotoData where
  show photo = "\t" ++ getInPath photo ++ " -> " ++ getOutPath photo ++
    if isInFile photo
        then
            " (Only rename)"
        else
            " (Must be moved)"

data PhotoHierarchy = Hierarchy { getLimits :: [(UTCTime, UTCTime)], getPath :: FilePath, getPictures :: [PhotoData] }
    
instance Ord PhotoHierarchy where
    (<=) _ hierarchy = takeFileName (getPath hierarchy) == "other"
    
instance Eq PhotoHierarchy where
    (==) hierarchyA hierarchyB = getPath hierarchyA == getPath hierarchyB

printEW :: String -> IO ()
printEW = hPutStrLn stderr

tryDo :: FilePath -> IO a -> IO (Maybe a)
tryDo path function = tryIOError function >>= evaluateExc path
  where
    evaluateExc _ (Right value) = pure $ Just value
    evaluateExc pathVal (Left exc)
            | isPermissionError exc = excPattern "Insufficient privileges to perform the operation." pathVal
            | isDoesNotExistError exc = excPattern "The directory/file does not exist." pathVal
            | isAlreadyInUseError exc = excPattern "File is already in use." pathVal
            | otherwise = excPattern "Unknown problem." pathVal
      where
        excPattern message pathValX = printEW ("Warning: " ++ message ++ " (" ++ pathValX ++ ")") >> pure Nothing

maybeList :: IO (Maybe [a]) -> IO [a]
maybeList value = value >>= maybe (return []) return

main :: IO ()
main = do
    (mode, (pathFrom, pathTo)) <- parseArguments
    case mode of
        CleanMode -> runWithClean pathFrom pathTo
        BasicMode -> runBasic pathFrom pathTo
        HelpMode -> printHelp
        DryRunMode -> printDryRun pathFrom pathTo
        PathErrorMode -> printEW "Error: Can't access directory. (DEST_DIR, SRC_DIR)" >> printHelp
        ErrorMode -> printEW "Error: Invalid type or amount of arguments." >> printHelp 
  where
    printHelp = putStrLn $ usageInfo "Usage : ./tidy [ARGS] SRC_DIR [DEST_DIR]" avalibeMods

runBasic :: FilePath -> FilePath -> IO ()
runBasic pathFrom pathTo = do
    hierarchy <- createFullHierarchy pathFrom pathTo
    hierarchyWithoutEmpty <- pure $ usedOrUnused (not . null) hierarchy
    mapM_ (\value -> tryDo (getPath value) (createDirectoryIfMissing True (getPath value))) hierarchyWithoutEmpty
    _ <- applyMove True hierarchyWithoutEmpty
    _ <- applyMove False hierarchyWithoutEmpty
    putStrLn "Info: Done!"
  where
    applyMove inFolder = mapM $ mapM (doRename inFolder) . getPictures
    doRename inFolder photo =
        if isInFile photo == inFolder 
            then 
                tryDo (getOutPath photo) $ renameFile (getInPath photo) (getOutPath photo)
            else 
                return Nothing

printDryRun :: FilePath -> FilePath -> IO ()
printDryRun pathFrom pathTo = do
    hie <- createFullHierarchy pathFrom pathTo
    unless (null (usedOrUnused null hie)) $ do
        putStr "\nUnused output folders: "
        putStrLn $ foldl (\first second -> first ++ show (getPath second) ++ " ") "" $ usedOrUnused null hie
    putStrLn "\nChanges to be done (Mode: dry-run):"
    putStrLn $ foldl (\str1 str2 -> str1 ++ "\n" ++ show (getPath str2) ++ ":" ++ printFold str2) "" $ usedOrUnused (not . null) hie
      where
        printFold value = foldl (\str1 str2 -> str1 ++ "\n" ++ show str2 ++ "\n") "" $ getPictures value

runWithClean :: FilePath -> FilePath -> IO ()
runWithClean pathFrom pathTo = runBasic pathFrom pathTo >> makeClean pathFrom >> putStrLn "Info: Cleaning done!"
  where
    makeClean path = do
        dirContentValX <- maybeList $ tryDo path $ listDirectory path
        subDirs <- filterM (\folder -> doesDirectoryExist (path </> folder)) dirContentValX
        unless (null subDirs) $ mapM_ (\name -> makeClean (path </> name)) subDirs
        dirContentValY <- maybeList $ tryDo path $ listDirectory path 
        when (null dirContentValY) $ tryDo path (removeDirectory path) >> putStrLn ("Info: Trying to remove: " ++ path)

createFullHierarchy :: FilePath -> FilePath -> IO [PhotoHierarchy]
createFullHierarchy from to = createHierarchyBase from to >>= mapM (\value -> insertExistingPhotos (getPath value) value)
  where
    insertExistingPhotos path value = do
        folderContent <- do
            doesExist <- doesDirectoryExist path
            if doesExist
                then
                   maybeList $ tryDo path $ listDirectory path
                else
                    return []
        photosWithPath <- separatePaths folderContent path
        hierarchyAdd <- createPhotos True removePrefix photosWithPath
        pure $ value { getPictures = addPrefix (sort (getPictures value ++ hierarchyAdd)) (getPath value) }
      where    
        removePrefix fileName = 
            if isNothing (readMaybe (takeWhile (/='_') fileName) :: Maybe Int)
                then
                    return fileName
                else
                    return $ tail $ dropWhile (/='_') fileName        
        addPrefix val pathX = zipWith (\photo i -> photo { getOutPath = pathX </> (i ++ getOutPath photo) }) val $ createIndexList val

createHierarchyBase :: FilePath -> FilePath -> IO [PhotoHierarchy]
createHierarchyBase path pathTo = do
    currentTime <- getCurrentTime
    configHierarchy <- maybeList $ parseConfigFile path pathTo
    photoData <- createPhotoData path
    hieB <- pure $ concateDuplicity $ configHierarchy ++ [Hierarchy [(currentTime, currentTime)] (pathTo </> "other") []]
    filterM (\valH -> havePerms (getPath valH) True) $ insertPhotos photoData hieB

createPhotoData :: FilePath -> IO [PhotoData]
createPhotoData path = getContent path >>= createPhotos False pure
  where
    getContent pathFolder = do
        permsBool <- havePerms pathFolder False
        if not permsBool
            then
                return []
            else do
                dirContentVal <- maybeList $ tryDo pathFolder $ listDirectory pathFolder 
                subDirs <- filterM (\folder -> doesDirectoryExist (pathFolder </> folder)) dirContentVal
                allFiles <- filterM (\file -> fmap not (doesDirectoryExist (pathFolder </> file))) dirContentVal
                if ".nomedia" `elem` allFiles
                    then 
                        return []
                    else
                        liftM2 (++) (separatePaths allFiles pathFolder) (foldTogether pathFolder subDirs)
    foldTogether pathZ dirs = foldl (liftM2 (++)) (pure []) $ map (\name -> getContent (pathZ </> name)) dirs

createPhotos :: Bool -> (FilePath -> IO FilePath) -> [FilePath] -> IO [PhotoData]
createPhotos isIn func paths = mapM (createPhoto func isIn) paths >>= filterM (pure . isJust) >>= mapM (pure . fromJust)
  where
    createPhoto fun inFol path = do
        modTime <- tryDo path $ getModificationTime path
        if isNothing modTime
            then
                return Nothing
            else
                Just <$> liftM4 Photo (pure path) (pure (fromJust modTime)) (fun (takeFileName path)) (pure inFol)

insertPhotos :: [PhotoData] -> [PhotoHierarchy] -> [PhotoHierarchy]  
insertPhotos _ [] = [] 
insertPhotos photo (x:xs) = x { getPictures = fst (partition (isInTime x) photo) } : insertPhotos (snd (partition (isInTime x) photo)) xs   
  where
    isInTime hierarchy photoData = 
        takeFileName (getPath hierarchy) == "other" || 
        any (\value -> fst value <= getPhotoTime photoData && snd value > getPhotoTime photoData) (getLimits hierarchy)

parseConfigFile :: FilePath -> FilePath -> IO (Maybe [PhotoHierarchy])
parseConfigFile path pathTo = tryDo path $ withFile (path </> "tidy.conf") ReadMode $ \handleVal -> do
    fileContent <- maybeList $ tryDo path $ hGetContents handleVal
    parsedData <- mapM (parseLine.words) $ lines fileContent
    evaluate $ sort $ map (\(Just from, Just to, pathF) -> Hierarchy [(from, to)] (pathTo </> pathF) []) $ filterInvalid parsedData
  where
    parseLine value = 
        if length value == 5
            then
                (\[t11, t12, t21, t22, file] -> pure (getTConfig (t11 ++ t12), getTConfig (t21 ++ t22), file)) value
            else
                printEW "Warning: Skiping invalid line in config..." >> pure (Nothing, Nothing, "")
    filterInvalid = filter $ \(val1, val2, _) -> isJust val1 && isJust val2
    getTConfig value = parseTimeM True defaultTimeLocale "%Y-%-m-%-d%R" value :: Maybe UTCTime

concateDuplicity :: [PhotoHierarchy] -> [PhotoHierarchy]
concateDuplicity [] = []
concateDuplicity (x:xs) =
    if x `elem` xs
        then
            takeWhile (/= x) xs ++ concateH x (head (dropWhile (/= x) xs)) : tail (dropWhile (/= x) xs)
        else
            x : concateDuplicity xs
  where
    concateH value hie = hie { getPictures = getPictures hie ++ getPictures value, getLimits = getLimits hie ++ getLimits value } 

usedOrUnused :: ([PhotoData] -> Bool) -> [PhotoHierarchy] -> [PhotoHierarchy]
usedOrUnused fun = filter $ fun . getPictures

createIndexList :: Foldable t => t a -> [String]
createIndexList val = [replicate (digitCounter 1 (length val) - digitCounter 1 pref) '0' ++ show pref ++ "_" | pref <- [0 .. length val - 1]]
  where        
    digitCounter base num =
        if num >= 10 
            then 
                digitCounter (base + 1) (div num 10) 
            else 
                base

separatePaths :: [FilePath] -> FilePath -> IO [FilePath]
separatePaths paths path = mapM (\photo -> pure (path </> photo)) $ filterExt paths
  where
    filterExt = filter (\file -> elem [toLower ext | ext <- takeExtension file] [".jpg", ".jpeg"])

havePerms :: FilePath -> Bool -> IO Bool
havePerms path checkIfEx = do
    dirEx <- doesDirectoryExist path
    if not dirEx && checkIfEx
        then
            return True
        else do
            perms <- tryDo path $ getPermissions path
            out <- pure $ isJust perms && writable (fromJust perms) && searchable (fromJust perms)
            unless out $ printEW $ "Warning: Cant't write or search. (" ++ path ++ ")"
            return out

avalibeMods :: [OptDescr ProgramMode]
avalibeMods = 
   [Option ['h'] ["help"] (NoArg HelpMode) "Show help", 
   Option ['c'] ["clean"] (NoArg CleanMode) "Remove all leftovers",
   Option ['n'] ["dry-run"] (NoArg DryRunMode) "Show changes (without changin anything)"] 

parseArguments :: IO (ProgramMode, (FilePath, FilePath))
parseArguments = do
    arguments <- getArgs
    case getOpt Permute avalibeMods arguments of
        (mode, paths, []) 
                    | HelpMode `elem` mode -> return (HelpMode, ("", ""))
                    | length mode <= 1 && (not . null) paths && length paths < 3 -> do 
                        resultData <- tryDo "./" getCurrentDirectory
                        if isNothing resultData 
                            then 
                                return (PathErrorMode, ("", "")) 
                            else
                                checkExis (mode ++ [BasicMode]) $ paths ++ [fromJust resultData]
                    | otherwise -> return (ErrorMode, ("", ""))        
        (_, _, err) -> printEW ("Error: " ++ concat err) >> return (ErrorMode, ("", ""))
  where
    checkExis modeVal pathsVal = do
        existFrom <- doesDirectoryExist $ head pathsVal
        existTo <- doesDirectoryExist $ (head . tail) pathsVal
        if not existFrom || not existTo
            then
                return (PathErrorMode, ("", ""))
            else
                return (head modeVal, (head pathsVal, (head . tail) pathsVal))
