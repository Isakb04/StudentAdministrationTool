{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Lib (
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
    addStudent,
    addModule,
    removeStudent,
    removeModule,
    exportStudents,
    exportModules,
    addStudentToModule,
    removeStudentFromModule
) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)
import Data.List (find, delete)
import System.Directory (createDirectoryIfMissing)

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

findStudentByFirstName :: String -> [Student] -> [Student]
findStudentByFirstName fName students = filter (\s -> firstName s == fName) students

findStudentByLastName :: String -> [Student] -> [Student]
findStudentByLastName lName students = filter (\s -> lastName s == lName) students

findStudentById :: Int -> [Student] -> Maybe Student
findStudentById sId students = find (\s -> studentId s == sId) students

findModuleByName :: String -> [Module] -> Maybe Module
findModuleByName mName modules = find (\m -> moduleName m == mName) modules

findModuleById :: Int -> [Module] -> Maybe Module
findModuleById mId modules = find (\m -> moduleId m == mId) modules

addStudent :: String -> String -> [Int] -> [Student] -> [Module] -> IO (Either String [Student])
addStudent fName lName moduleIds students modules = do
    return $ if all (`elem` map moduleId modules) moduleIds then
        Right (newStudent : students)
    else
        Left "One or more module IDs are invalid."
    where
        newId = if null students then 1 else maximum (map studentId students) + 1
        newStudent = Student newId fName lName moduleIds
        
addModule :: String -> [Module] -> IO (Either String [Module])
addModule mName modules = do
    return $ if any (\m -> moduleName m == mName) modules then
        Left "Module already exists."
    else
        Right (newModule : modules)
    where
        newId = if null modules then 1 else maximum (map moduleId modules) + 1
        newModule = Module newId mName []

removeStudent :: Int -> [Student] -> [Module] -> IO (Either String ([Student], [Module]))
removeStudent sId students modules = do
    case findStudentById sId students of
        Just student -> return $ Right (delete student students, updatedModules)
        Nothing -> return $ Left "No student found with given ID"
    where
        updatedModules = map (\m -> m { enrolledStudents = filter (/= sId) (enrolledStudents m) }) modules

removeModule :: Int -> [Module] -> [Student] -> IO (Either String ([Module], [Student]))
removeModule mId modules students = do
    case findModuleById mId modules of
        Just moduleToRemove -> return $ Right (delete moduleToRemove modules, updatedStudents)
        Nothing -> return $ Left "No module found with given ID"
    where
        updatedStudents = map (\s -> s { enrolledModules = filter (/= mId) (enrolledModules s) }) students

addStudentToModule :: Int -> Int -> [Student] -> [Module] -> IO (Either String ([Student], [Module]))
addStudentToModule sId mId students modules = do
    case findStudentById sId students of
        Just _ -> 
            case findModuleById mId modules of
                Just _ -> return $ Right (updatedStudents, updatedModules)            
                Nothing -> return $ Left "No module found with given ID"
        Nothing -> return $ Left "No student found with given ID"
    where
        updatedStudents = map (\s -> if studentId s == sId then s { enrolledModules = mId : enrolledModules s } else s) students
        updatedModules = map (\m -> if moduleId m == mId then m { enrolledStudents = sId : enrolledStudents m } else m) modules

removeStudentFromModule :: Int -> Int -> [Student] -> [Module] -> IO (Either String ([Student], [Module]))
removeStudentFromModule sId mId students modules = do
    case findStudentById sId students of
        Just _ -> 
            case findModuleById mId modules of
                Just _ -> return $ Right (updatedStudents, updatedModules)
                Nothing -> return $ Left "No module found with given ID"
        Nothing -> return $ Left "No student found with given ID"
    where
        updatedStudents = map (\s -> if studentId s == sId then s { enrolledModules = filter (/= mId) (enrolledModules s) } else s) students
        updatedModules = map (\m -> if moduleId m == mId then m { enrolledStudents = filter (/= sId) (enrolledStudents m) } else m) modules
        
exportStudents :: [Student] -> IO ()
exportStudents students = do
    let content = unlines $ map (\s -> "Student ID: " ++ show (studentId s) ++ ", Name: " ++ firstName s ++ " " ++ lastName s ++ ", Modules: " ++ show (enrolledModules s)) students
    createDirectoryIfMissing True "exports"
    writeFile "exports/students_list.txt" content
    putStrLn "Students exported successfully."

exportModules :: [Module] -> IO ()
exportModules modules = do
    let content = unlines $ map (\m -> "Module ID: " ++ show (moduleId m) ++ ", Name: " ++ moduleName m ++ ", Students: " ++ show (enrolledStudents m)) modules
    createDirectoryIfMissing True "exports"
    writeFile "exports/modules_list.txt" content
    putStrLn "Modules exported successfully."