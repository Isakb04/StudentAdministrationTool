{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)

data Student = Student
    { studentId :: Int
    , firstName :: String
    , lastName :: String
    , enrolledModules :: [Int]
    } deriving (Show, Eq, ToJSON, FromJSON, Generic)

data Module = Module
    { moduleId :: Int
    , moduleName :: String
    , enrolledStudents :: Int
    } deriving (Show, Eq, ToJSON, FromJSON, Generic)    

studentFile :: FilePath
studentFile = "students.json"

moduleFile :: FilePath
moduleFile = "modules.json"

loadStudents :: IO (Either String [Student])
loadStudents = do
    studentData <- B.readFile studentFile
    return $ eitherDecode studentData

loadModules :: IO (Either String [Module])
loadModules = do
    moduleData <- B.readFile moduleFile
    return $ eitherDecode moduleData

help :: IO ()
help = do
    putStrLn "Commands:"
    putStrLn "findStudentByFirstName: Find a student by first name"
    putStrLn " findStudentByLastName: Find a student by last name"
    putStrLn "       findStudentById: Find a student by ID"
    putStrLn "      findModuleByName: Find a module by name"
    putStrLn "        findModuleById: Find a module by ID"
    putStrLn "                   add: Add a student or module"
    putStrLn "                remove: Remove a student or module"
    putStrLn "                   :q : Exit the program"



findStudentByFirstName :: String -> IO (Either String [Student])
findStudentByFirstName fName = do
    eitherStudents <- loadStudents
    return $ case eitherStudents of
        Left err -> Left err
        Right students -> 
            let filteredStudents = filter (\s -> firstName s == fName) students
            in if null filteredStudents
               then Left "No student found with given first name"
               else Right filteredStudents

findStudentByLastName :: String -> IO (Either String [Student])
findStudentByLastName lName = do
    eitherStudents <- loadStudents
    return $ case eitherStudents of
        Left err -> Left err
        Right students -> 
            let filteredStudents = filter (\s -> lastName s == lName) students
            in if null filteredStudents
               then Left "No student found with given last name"
               else Right filteredStudents

findStudentById :: Int -> IO (Either String Student)
findStudentById sId = do
    eitherStudents <- loadStudents
    return $ case eitherStudents of
        Left err -> Left err
        Right students -> case filter (\s -> studentId s == sId) students of
            [] -> Left "No student found with given ID"
            (x:_) -> Right x

findModuleById :: Int -> IO (Either String Module)
findModuleById mId = do
    eitherModules <- loadModules
    return $ case eitherModules of
        Left err -> Left err
        Right modules -> case filter (\m -> moduleId m == mId) modules of
            [] -> Left "No module found with given ID"
            (x:_) -> Right x

addStudent :: IO (Either String [Student])
addStudent = do
    putStrLn "Enter the first name of the student:"
    fName <- getLine
    putStrLn "Enter the last name of the student:"
    lName <- getLine
    putStrLn "Enter the enrolled modules (comma-separated module IDs):"
    modulesInput <- getLine
    let modules = map read $ words $ map (\c -> if c == ',' then ' ' else c) modulesInput
    eitherStudents <- loadStudents
    case eitherStudents of
        Left err -> return $ Left err
        Right students -> do
            let newId = if null students then 1 else 1 + maximum (map studentId students)
            let newStudent = Student newId fName lName modules
            putStrLn "Are you sure you want to add this student? (Y/N)"
            confirmation <- getLine
            if confirmation == "Y" || confirmation == "y"
                then return $ Right (newStudent : students)
                else return $ Left "Addition cancelled."

addModule :: IO (Either String [Module])
addModule = do
    putStrLn "Enter the name of the module:"
    mName <- getLine
    putStrLn "Enter the number of enrolled students:"
    studentsInput <- getLine
    let students = read studentsInput :: Int  -- Correctly read the number of students as an Int
    eitherModules <- loadModules
    case eitherModules of
        Left err -> return $ Left err
        Right modules -> do
            let newId = if null modules then 1 else 1 + maximum (map moduleId modules)
            let newModule = Module newId mName students
            return $ Right (newModule : modules)

data RemovalResult = StudentRemoval (Either String [Student]) | ModuleRemoval (Either String [Module])

remove :: IO RemovalResult
remove = do
    putStrLn "Do you want to remove a Student or a Module? (Type 'Student' or 'Module')"
    entityType <- getLine
    case entityType of
        "Student" -> StudentRemoval <$> removeStudent
        "Module" -> ModuleRemoval <$> removeModule
        _ -> return $ StudentRemoval $ Left "Invalid entity type. Please type 'Student' or 'Module'"

removeStudent :: IO (Either String [Student])
removeStudent = do
    putStrLn "Enter the first name of the student:"
    fName <- getLine
    putStrLn "Enter the last name of the student:"
    lName <- getLine
    eitherStudents <- loadStudents
    case eitherStudents of
        Left err -> return $ Left err
        Right students -> do
            let filteredStudents = filter (\s -> firstName s == fName && lastName s == lName) students
            if null filteredStudents
                then return $ Left "No student found with given name"
                else do
                    putStrLn "Are you sure you want to remove this student? (Y/N)"
                    confirmation <- getLine
                    if confirmation == "Y" || confirmation == "y"
                        then return $ Right $ filter (\s -> not (firstName s == fName && lastName s == lName)) students
                        else return $ Left "Removal cancelled."

removeModule :: IO (Either String [Module])
removeModule = do
    putStrLn "Enter the name of the module:"
    mName <- getLine
    eitherModules <- loadModules
    case eitherModules of
        Left err -> return $ Left err
        Right modules -> do
            let filteredModules = filter (\m -> moduleName m == mName) modules
            if null filteredModules
                then return $ Left "No module found with given name"
                else do
                    putStrLn "Are you sure you want to remove this module? (Y/N)"
                    confirmation <- getLine
                    if confirmation == "Y" || confirmation == "y"
                        then return $ Right $ filter (\m -> not (moduleName m == mName)) modules
                        else return $ Left "Removal cancelled."

data ExportResult = StudentsExported | ModulesExported | BothExported | Error String

export :: IO ExportResult
export = do
    putStrLn "Do you want to export the students or the modules? (Type 'Students' or 'Modules')"
    entityType <- getLine
    case entityType of
        "Students" -> return StudentsExported
        "Modules" -> return ModulesExported
        _ -> return $ Error "Invalid entity type. Please type 'Students' or 'Modules'"
        
exportStudents :: IO ()
exportStudents = do
    eitherStudents <- loadStudents
    case eitherStudents of
        Left err -> putStrLn $ "Failed to load students: " ++ err
        Right students -> do
            let studentLines = map (\s -> "Student ID: " ++ show (studentId s) ++ ", Name: " ++ firstName s ++ " " ++ lastName s ++ ", Modules: " ++ show (enrolledModules s)) students
            writeFile "students_list.txt" (unlines studentLines)
            putStrLn "Students have been successfully written to 'students_list.txt'."

exportModules :: IO ()
exportModules = do
    eitherModules <- loadModules
    case eitherModules of
        Left err -> putStrLn $ "Failed to load modules: " ++ err
        Right modules -> do
            let moduleLines = map (\m -> "Module ID: " ++ show (moduleId m) ++ ", Name: " ++ moduleName m ++ ", Students: " ++ show (enrolledStudents m)) modules
            writeFile "modules_list.txt" (unlines moduleLines)
            putStrLn "Modules have been successfully written to 'modules_list.txt'."

