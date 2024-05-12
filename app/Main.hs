{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Main (
    Student(..),
    Module(..),
    studentFile,
    moduleFile,
    loadStudents,
    loadModules,
    saveStudents,
    saveModules,
    findStudentByFirstName,
    findStudentByLastName,
    findStudentById,
    findModuleByName,
    findModuleById,
    add,
    remove,
    export,
    exportStudents,
    exportModules,
    main
) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)


-- Define the main function
main :: IO ()
main = do
    help

data Student = Student
    { studentId :: Int
    , firstName :: String
    , lastName :: String
    , enrolledModules :: [Int]
    } deriving (Show, Eq, ToJSON, FromJSON, Generic)

data Module = Module
    { moduleId :: Int
    , moduleName :: String
    , enrolledStudents :: [Int]  
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

saveStudents :: [Student] -> IO ()
saveStudents students = B.writeFile studentFile (encodePretty students)

saveModules :: [Module] -> IO ()
saveModules modules = B.writeFile moduleFile (encodePretty modules)

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
    putStrLn "                  export: Export the students or modules"
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

findModuleByName :: String -> IO (Either String Module)
findModuleByName mName = do
    eitherModules <- loadModules
    return $ case eitherModules of
        Left err -> Left err
        Right modules -> case filter (\m -> moduleName m == mName) modules of
            [] -> Left "No module found with given name"
            (x:_) -> Right x

data AddResult = StudentAdded (Either String [Student]) | ModuleAdded (Either String [Module])

add :: IO AddResult
add = do
    putStrLn "Do you want to add a Student or a Module? (Type 'Student' or 'Module')"
    entityType <- getLine
    case entityType of
        "Student" -> StudentAdded <$> addStudent
        "student" -> StudentAdded <$> addStudent
        "Module" -> ModuleAdded <$> addModule
        "module" -> ModuleAdded <$> addModule
        _ -> do
            putStrLn "Invalid entity type. Please type 'Student' or 'Module'"
            return $ StudentAdded $ Left "Invalid entity type. Please type 'Student' or 'Module'"

addStudent :: IO (Either String [Student])
addStudent = do
    putStrLn "Enter the first name of the student:"
    fName <- getLine
    putStrLn "Enter the last name of the student:"
    lName <- getLine
    putStrLn "Enter the enrolled modules (comma-separated module IDs):"
    modulesInput <- getLine
    let moduleIds = map read $ words $ map (\c -> if c == ',' then ' ' else c) modulesInput  -- Convert comma-separated string to list of Ints
    eitherModules <- loadModules
    eitherStudents <- loadStudents
    case (eitherModules, eitherStudents) of
        (Left modErr, _) -> do
            putStrLn "Failed to load modules."
            return $ Left modErr
        (_, Left studErr) -> do
            putStrLn "Failed to load students."
            return $ Left studErr
        (Right modules, Right students) -> do
            let validModuleIds = map moduleId modules
            putStrLn $ "Valid module IDs: " ++ show validModuleIds  -- Debug statement
            putStrLn $ "Provided module IDs: " ++ show moduleIds  -- Debug statement
            let areAllValid = all (`elem` validModuleIds) moduleIds
            if not areAllValid
                then do
                    putStrLn "One or more module IDs are invalid."
                    return $ Left "One or more module IDs are invalid."
                else do
                    let newId = if null students then 1 else 1 + maximum (map studentId students)
                    let newStudent = Student newId fName lName moduleIds
                    putStrLn "Are you sure you want to add this student? (Y/N)"
                    confirmation <- getLine
                    if confirmation == "Y" || confirmation == "y"
                        then do
                            let updatedModules = map (\m -> if moduleId m `elem` moduleIds then m { enrolledStudents = studentId newStudent : enrolledStudents m } else m) modules
                            let updatedStudents = newStudent : students
                            saveStudents updatedStudents  -- Save the updated list to the JSON file
                            saveModules updatedModules  -- Save the updated modules to the JSON file
                            return $ Right updatedStudents
                        else do
                            putStrLn "Addition cancelled."
                            return $ Left "Addition cancelled."
                            
addModule :: IO (Either String [Module])
addModule = do
    putStrLn "Enter the name of the module:"
    mName <- getLine
    eitherModules <- loadModules
    eitherStudents <- loadStudents
    case (eitherModules, eitherStudents) of
        (Left modErr, _) -> return $ Left modErr
        (_, Left studErr) -> return $ Left studErr
        (Right modules, Right students) -> do
            if any (\m -> moduleName m == mName) modules
                then do 
                    putStrLn "Module already exists."
                    return $ Left "Module already exists."
                else do
                    let newId = if null modules then 1 else 1 + maximum (map moduleId modules)
                    let studentIds = [studentId s | s <- students, newId `elem` enrolledModules s]
                    let newModule = Module newId mName studentIds
                    putStrLn "Are you sure you want to add this module? (Y/N)"
                    confirmation <- getLine
                    if confirmation == "Y" || confirmation == "y"
                        then do
                            let updatedModules = newModule : modules
                            saveModules updatedModules  -- Save the updated list to the JSON file
                            return $ Right updatedModules
                        else do
                            putStrLn "Addition cancelled."  -- Ensure this gets printed
                            return $ Left "Addition cancelled."


data RemovalResult = StudentRemoval (Either String [Student]) | ModuleRemoval (Either String [Module])

remove :: IO RemovalResult
remove = do
    putStrLn "Do you want to remove a Student or a Module? (Type 'Student' or 'Module')"
    entityType <- getLine
    case entityType of
        "Student" -> StudentRemoval <$> removeStudent
        "student" -> StudentRemoval <$> removeStudent
        "Module" -> ModuleRemoval <$> removeModule
        "module" -> ModuleRemoval <$> removeModule
        _ -> do
            putStrLn "Invalid entity type. Please type 'Student' or 'Module'"
            return $ StudentRemoval $ Left "Invalid entity type. Please type 'Student' or 'Module'"

removeStudent :: IO (Either String [Student])
removeStudent = do
    putStrLn "Enter the first and last name of the student or just the student ID:"
    input <- getLine
    eitherStudents <- loadStudents
    eitherModules <- loadModules
    case (eitherStudents, eitherModules) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right students, Right modules) -> do
            let filteredStudents = filter (\s -> (firstName s ++ " " ++ lastName s == input) || show (studentId s) == input) students
            if null filteredStudents
                then do 
                    putStrLn "No student found with given name or ID"
                    return $ Left "No student found with given name or ID"
                else do
                    putStrLn "Are you sure you want to remove this student? (Y/N)"
                    confirmation <- getLine
                    if confirmation == "Y" || confirmation == "y"
                    then do 
                        let studentToRemove = head filteredStudents
                        let updatedStudents = filter (/= studentToRemove) students
                        let updatedModules = map (\m -> m { enrolledStudents = filter (/= studentId studentToRemove) (enrolledStudents m) }) modules
                        saveStudents updatedStudents  -- Save the updated list to the JSON file
                        saveModules updatedModules  -- Update the modules JSON file
                        return $ Right updatedStudents
                    else do
                        putStrLn "Removal cancelled."
                        return $ Left "Removal cancelled."
                        

removeModule :: IO (Either String [Module])
removeModule = do
    putStrLn "Enter the name or ID of the module:"
    input <- getLine
    eitherModules <- loadModules
    eitherStudents <- loadStudents
    case (eitherModules, eitherStudents) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right modules, Right students) -> do
            let filteredModules = filter (\m -> moduleName m == input || show (moduleId m) == input) modules
            if null filteredModules
                then do
                    putStrLn "No module found with given name or ID"
                    return $ Left "No module found with given name or ID"
                else do
                    putStrLn "Are you sure you want to remove this module? (Y/N)"
                    confirmation <- getLine
                    if confirmation == "Y" || confirmation == "y"
                    then do 
                        let moduleToRemove = head filteredModules
                        let updatedModules = filter (/= moduleToRemove) modules
                        let updatedStudents = map (\s -> s { enrolledModules = filter (/= moduleId moduleToRemove) (enrolledModules s) }) students
                        saveModules updatedModules  -- Save the updated list to the JSON file
                        saveStudents updatedStudents  -- Update the students JSON file
                        return $ Right updatedModules
                    else do
                        putStrLn "Removal cancelled."
                        return $ Left "Removal cancelled."

data ExportResult = StudentsExported | ModulesExported | Error String

export :: IO ExportResult
export = do
    putStrLn "Do you want to export the students or the modules? (Type 'Students' or 'Modules')"
    entityType <- getLine
    case entityType of
        "Students" -> return StudentsExported
        "Modules" -> return ModulesExported
        _ -> do
            putStrLn "Invalid entity type. Please type 'Students' or 'Modules'"
            return $ Error "Invalid entity type. Please type 'Students' or 'Modules'"
        
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