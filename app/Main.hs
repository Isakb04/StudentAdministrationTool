module Main (
    main,  -- Export only the main function
    commandSystem,  -- Optionally export more identifiers as needed
    help,  -- Export help function
    add,  -- Export add function
    remove,  -- Export remove function
    findStudent,  -- Export findStudent function
    findModule,  -- Export findModule function
    exportData,  -- Export exportData function
    config  -- Export config function
) where

-- Import necessary modules
import Lib
import Data.Maybe (maybeToList)
import Data.Char (toUpper)
import System.Environment()

-- Main function
main :: IO ()
main = do
    putStrLn "[Student and Module Management System!]"
    commandSystem
    return ()

-- Command system
commandSystem :: IO ()
commandSystem = do
    putStrLn ""
    putStrLn "now in Interface mode"
    putStrLn ""
    putStrLn "   [Command options]"
    putStrLn "1: Add Student or Module"
    putStrLn "2: Remove Student or Module"
    putStrLn "3: Find Student"
    putStrLn "4: Find Module"
    putStrLn "5: Export Data"
    putStrLn "0: Exit"
    putStrLn ""
    putStrLn "Enter command number:"
    cmd <- getLine
    case cmd of
        "1" -> add >> commandSystem
        "2" -> remove >> commandSystem
        "3" -> findStudent >> commandSystem
        "4" -> findModule >> commandSystem
        "5" -> exportData >> commandSystem
        "0" -> do
            putStrLn ""
            putStrLn "Exiting the command system, now in default mode."
            putStrLn ""
        _ -> do
            putStrLn "Invalid command. Please try again."
            commandSystem

-- Help function
help :: IO ()
help = do
    putStrLn "               [Commands]"
    putStrLn "commandSystem: Enter the command system"
    putStrLn "          add: addStudent <firstName> <lastName> <modules> sepertaed by comma: Adds a new student."
    putStrLn "               addModule <moduleName>: Adds a new module."
    putStrLn "       remove: removeStudent <studentId>: Removes a student by ID."
    putStrLn "               removeModule <moduleId>: Removes a module by ID."
    putStrLn "       config: Add <studentId> <moduleId>: Adds a student to a module."
    putStrLn "               Remove <studentId> <moduleId>: Removes a student from a module."
    putStrLn "  findStudent: findStudent <info>: Finds a student by first name, last name, or ID."
    putStrLn "   findModule: findModule <info>: Finds a module by name or ID."
    putStrLn "       export: exportStudents <Students>: Exports the list of students to the specified file."
    putStrLn "               exportModules <Modules>: Exports the list of modules to the specified file."
    putStrLn "          :q : Exit"
    putStrLn ""
    
-- Add a new student or module
add :: IO ()
add = do
    putStrLn "[add new student or module]"
    entityType <- getLine
    case (toUpper (head entityType) : tail entityType) of
        "Student" -> do
            fName <- getLine
            lName <- getLine
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
            putStrLn "Invalid arguments. Please refer to the README or help command."
            

-- Remove a student or module
remove :: IO ()
remove = do
    putStrLn "[remove student or module]"
    entityType <- getLine
    case (toUpper (head entityType) : tail entityType) of
        "Student" -> do
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
            putStrLn "Invalid arguments. Please refer to the README or help command."
            

-- Add or remove a student from a module
config :: IO ()
config = do
    putStrLn "[add or remove student from module]"
    entityType <- getLine
    case (toUpper (head entityType) : tail entityType) of
        "Add" -> do
            sIdInput <- getLine
            let sId = read sIdInput
            mIdInput <- getLine
            let mId = read mIdInput
            eitherModules <- loadModules
            eitherStudents <- loadStudents
            case (eitherModules, eitherStudents) of
                (Right modules, Right students) -> do
                    result <- addStudentToModule sId mId students modules
                    case result of
                        Left err -> putStrLn err
                        Right (updatedStudents, updatedModules) -> do
                            putStrLn "Are you sure you want to add this student to the module? (Y/N)"
                            confirmation <- getLine
                            if confirmation == "Y" || confirmation == "y"
                                then do
                                    saveStudents updatedStudents
                                    saveModules updatedModules
                                    putStrLn "Student added to module successfully!"
                                else putStrLn "Student addition to module cancelled."
                _ -> putStrLn "Error loading data."
        "Remove" -> do
            sIdInput <- getLine
            let sId = read sIdInput
            mIdInput <- getLine
            let mId = read mIdInput
            eitherModules <- loadModules
            eitherStudents <- loadStudents
            case (eitherModules, eitherStudents) of
                (Right modules, Right students) -> do
                    result <- removeStudentFromModule sId mId students modules
                    case result of
                        Left err -> putStrLn err
                        Right (updatedStudents, updatedModules) -> do
                            putStrLn "Are you sure you want to remove this student from the module? (Y/N)"
                            confirmation <- getLine
                            if confirmation == "Y" || confirmation == "y"
                                then do
                                    saveStudents updatedStudents
                                    saveModules updatedModules
                                    putStrLn "Student removed from module successfully!"
                                else putStrLn "Student removal from module cancelled."
                _ -> putStrLn "Error loading data."
        _ -> do
            putStrLn "Invalid arguments. Please refer to the README or help command."

-- Find a student
findStudent :: IO ()
findStudent = do
    putStrLn "[Find Student]"
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

-- Find a module
findModule :: IO ()
findModule = do
    putStrLn "[Find Module]"
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

-- Export data
exportData :: IO ()
exportData = do
    putStrLn "[Export Data]"
    entityType <- getLine
    case (toUpper (head entityType) : tail entityType) of
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
            putStrLn "Invalid input. Please refer to the README or help command."
