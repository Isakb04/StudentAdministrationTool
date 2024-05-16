module Main (
    main,  -- Export only the main function
    commandSystem,  -- Optionally export more identifiers as needed
    add,  -- Export add function
    remove,  -- Export remove function
    findStudent,  -- Export findStudent function
    findModule,  -- Export findModule function
    exportData  -- Export exportData function
) where

import Lib
import Data.Maybe (maybeToList)
import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "Welcome to the Student and Module Management System!"
    commandSystem
    help

commandSystem :: IO ()
commandSystem = do
    putStrLn ""
    putStrLn "Please enter a command:"
    putStrLn "1: Add Student or Module"
    putStrLn "2: Remove Student or Module"
    putStrLn "3: Find Student"
    putStrLn "4: Find Module"
    putStrLn "5: Export Data"
    putStrLn "0: Exit"
    putStrLn ""
    cmd <- getLine
    case cmd of
        "1" -> add
        "2" -> remove
        "3" -> findStudent
        "4" -> findModule
        "5" -> exportData
        "0" -> putStrLn "Exiting the command system."
        _ -> do
            putStrLn "Invalid command. Please try again."
            commandSystem

help :: IO ()
help = do
    putStrLn "Commands:"
    putStrLn "commandSystem: Enter the command system"
    putStrLn "          add: Add Student or Module"
    putStrLn "       remove: Remove Student or Module"
    putStrLn "  findStudent: Find Student"
    putStrLn "   findModule: Find Module"
    putStrLn "       export: Export Data"
    putStrLn "          :q : Exit"
    putStrLn ""
    

add :: IO ()
add = do
    putStrLn "Please enter the type of entity to add (Student or Module) **Case Sensitive**:"
    entityType <- getLine
    case entityType of
        "Student" -> do
            putStrLn "Enter the first name of the student:"
            fName <- getLine
            putStrLn "Enter the last name of the student:"
            lName <- getLine
            putStrLn "Enter the enrolled modules (comma-separated module IDs):"
            modulesInput <- getLine
            let moduleIds = map read $ words $ map (\c -> if c == ',' then ' ' else c) modulesInput
            eitherModules <- loadModules
            eitherStudents <- loadStudents
            case (eitherModules, eitherStudents) of
                (Right modules, Right students) -> do
                    result <- addStudent (toUpper (head fName) : tail fName) (toUpper (head lName) : tail lName) moduleIds students modules
                    case result of
                        Left err -> putStrLn err
                        Right updatedStudents -> do
                            let newStudent = head updatedStudents
                            let updatedModules = map (\m -> if moduleId m `elem` moduleIds then m { enrolledStudents = studentId newStudent : enrolledStudents m } else m) modules
                            putStrLn "Are you sure you want to add this student? (Y/N)"
                            confirmation <- getLine
                            if confirmation == "Y" || confirmation == "y"
                                then do
                                    saveStudents updatedStudents
                                    saveModules updatedModules
                                    putStrLn "Student added successfully!"
                                else putStrLn "Student addition cancelled."
                _ -> putStrLn "Error loading data."
        "Module" -> do
            putStrLn "Enter the name of the module:"
            mName <- getLine
            eitherModules <- loadModules
            case eitherModules of
                Right modules -> do
                    result <- addModule (toUpper (head mName) : tail mName) modules
                    case result of
                        Left err -> putStrLn err
                        Right updatedModules -> do
                            putStrLn "Are you sure you want to add this module? (Y/N)"
                            confirmation <- getLine
                            if confirmation == "Y" || confirmation == "y"
                                then do
                                    saveModules updatedModules
                                    putStrLn "Module added successfully!"
                                else putStrLn "Module addition cancelled."
                Left err -> putStrLn err
        _ -> do
            putStrLn "Invalid input. Please type 'Student' or 'Module'."
            add

remove :: IO ()
remove = do
    putStrLn "Please enter the type of entity to remove (Student or Module) **Case Sensitive**:"
    entityType <- getLine
    case entityType of
        "Student" -> do
            putStrLn "Enter the student ID to remove:"
            sIdInput <- getLine
            let sId = read sIdInput
            eitherModules <- loadModules
            eitherStudents <- loadStudents
            case (eitherModules, eitherStudents) of
                (Right modules, Right students) -> do
                    result <- removeStudent sId students modules
                    case result of
                        Left err -> putStrLn err
                        Right (updatedStudents, updatedModules) -> do
                            putStrLn "Are you sure you want to remove this student? (Y/N)"
                            confirmation <- getLine
                            if confirmation == "Y" || confirmation == "y"
                                then do
                                    saveStudents updatedStudents
                                    saveModules updatedModules
                                    putStrLn "Student removed successfully!"
                                else putStrLn "Student removal cancelled."
                _ -> putStrLn "Error loading data."
        "Module" -> do
            putStrLn "Enter the module ID to remove:"
            mIdInput <- getLine
            let mId = read mIdInput
            eitherModules <- loadModules
            eitherStudents <- loadStudents
            case (eitherModules, eitherStudents) of
                (Right modules, Right students) -> do
                    result <- removeModule mId modules students
                    case result of
                        Left err -> putStrLn err
                        Right (updatedModules, updatedStudents) -> do
                            putStrLn "Are you sure you want to remove this module? (Y/N)"
                            confirmation <- getLine
                            if confirmation == "Y" || confirmation == "y"
                                then do
                                    saveModules updatedModules
                                    saveStudents updatedStudents
                                    putStrLn "Module removed successfully!"
                                else putStrLn "Module removal cancelled."
                _ -> putStrLn "Error loading data."
        _ -> do
            putStrLn "Invalid input. Please type 'Student' or 'Module'."
            remove

findStudent :: IO ()
findStudent = do
    putStrLn "Enter the student's first name, last name or ID:"
    name <- getLine
    eitherStudents <- loadStudents
    case eitherStudents of
        Right students -> do
            let foundByFirstName = findStudentByFirstName (toUpper (head name) : tail name) students
            let foundByLastName = findStudentByLastName (toUpper (head name) : tail name) students
            let foundById = maybeToList $ findStudentById (read name) students
            putStrLn ""
            putStrLn "Students found by first name:"
            mapM_ (putStrLn . show) foundByFirstName
            putStrLn ""
            putStrLn "Students found by last name:"
            mapM_ (putStrLn . show) foundByLastName
            putStrLn ""
            putStrLn "Students found by ID:"
            mapM_ (putStrLn . show) foundById
        Left err -> putStrLn err

findModule :: IO ()
findModule = do
    putStrLn "Enter the module name or ID:"
    input <- getLine
    eitherModules <- loadModules
    case eitherModules of
        Right modules -> do
            let foundByName = findModuleByName (toUpper (head input) : tail input) modules
            let foundById = maybeToList $ findModuleById (read input) modules
            putStrLn ""
            putStrLn "Module found by name:"
            mapM_ (putStrLn . show) foundByName
            putStrLn ""
            putStrLn "Module found by ID:"
            mapM_ (putStrLn . show) foundById
        Left err -> putStrLn err

exportData :: IO ()
exportData = do
    putStrLn "Do you want to export students or modules? (Type 'Students' or 'Modules') **Case Sensitive**:"
    entityType <- getLine
    case entityType of
        "Students" -> do
            eitherStudents <- loadStudents
            case eitherStudents of
                Right students -> do
                    exportStudents students
                    putStrLn "Students exported successfully."
                Left err -> putStrLn err
        "Modules" -> do
            eitherModules <- loadModules
            case eitherModules of
                Right modules -> do
                    exportModules modules
                    putStrLn "Modules exported successfully."
                Left err -> putStrLn err
        _ -> do
            putStrLn "Invalid input. Please type 'Students' or 'Modules'."
            exportData
