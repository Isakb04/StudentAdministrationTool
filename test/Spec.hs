import Test.HUnit
import Lib
import System.Directory

tests :: [Test]
tests = 
    [ testFindStudentByFirstName
    , testFindStudentByLastName
    , testFindStudentById
    , testFindModuleByName
    , testFindModuleById
    , testAddStudent
    , testAddModule
    , testRemoveStudent
    , testRemoveModule
    , testExportStudents
    , testExportModules
    , testAddStudentToModule
    , testRemoveStudentFromModule
    , testLoadStudents
    , testLoadModules
    ]

-- Define the tests
testLoadStudents :: Test
testLoadStudents = TestCase $ do
    result <- loadStudents
    assertBool "Should load students.json" (case result of
        Left _ -> False
        Right _ -> True)

testLoadModules :: Test
testLoadModules = TestCase $ do
    result <- loadModules
    assertBool "Should load modules.json" (case result of
        Left _ -> False
        Right _ -> True)

testFindStudentByFirstName :: Test
testFindStudentByFirstName = TestCase $ do
    let students = [Student 1 "Test Alice" "Smith" [101, 102, 103], Student 2 "Test Bob" "Johnson" [101, 105]]
    assertEqual "Should find student by first name" [Student 1 "Test Alice" "Smith" [101, 102, 103]] (findStudentByFirstName "Test Alice" students)

testFindStudentByLastName :: Test
testFindStudentByLastName = TestCase $ do
    let students = [Student 1 "Test Alice" "Smith" [101, 102, 103], Student 2 "Test Bob" "Johnson" [101, 105]]
    assertEqual "Should find student by last name" [Student 2 "Test Bob" "Johnson" [101, 105]] (findStudentByLastName "Johnson" students)

testFindStudentById :: Test
testFindStudentById = TestCase $ do
    let students = [Student 1 "Test Alice" "Smith" [101, 102, 103], Student 2 "Test Bob" "Johnson" [101, 105]]
    assertEqual "Should find student by ID" (Just (Student 1 "Test Alice" "Smith" [101, 102, 103])) (findStudentById 1 students)

testFindModuleByName :: Test
testFindModuleByName = TestCase $ do
    let modules = [Module 101 "Test1" [101, 102, 103, 105], Module 102 "Test2" [101, 102, 103, 105]]
    assertEqual "Should find module by name" (Just (Module 101 "Test1" [101, 102, 103, 105])) (findModuleByName "Test1" modules)

testFindModuleById :: Test
testFindModuleById = TestCase $ do
    let modules = [Module 101 "Test1" [101, 102, 103, 105], Module 102 "Test2" [101, 102, 103, 105]]
    assertEqual "Should find module by ID" (Just (Module 101 "Test1" [101, 102, 103, 105])) (findModuleById 101 modules)

testAddStudent :: Test
testAddStudent = TestCase $ do
    let initialStudents = []
    let initialModules = [Module 101 "Test1" [], Module 102 "Test2" []]
    result <- addStudent "Test Alice" "Smith" [101, 102] initialStudents initialModules
    assertEqual "Should add a new student" (Right [Student 1 "Test Alice" "Smith" [101, 102]]) result

testAddModule :: Test
testAddModule = TestCase $ do
    let initialModules = []
    result <- addModule "Test1" initialModules
    assertEqual "Should add a new module" (Right [Module 1 "Test1" []]) result

testRemoveStudent :: Test
testRemoveStudent = TestCase $ do
    let students = [Student 1 "Test Alice" "Smith" [101, 102], Student 2 "Test Bob" "Johnson" [103]]
    let modules = [Module 101 "Test1" [], Module 102 "Test2" [], Module 103 "Test3" []]
    result <- removeStudent 1 students modules
    assertEqual "Should remove a student" (Right ([Student 2 "Test Bob" "Johnson" [103]], modules)) result

testRemoveModule :: Test
testRemoveModule = TestCase $ do
    let modules = [Module 1 "Test1" [], Module 2 "Test2" []]
    let students = []
    result <- removeModule 1 modules students
    assertEqual "Should remove a module" (Right ([Module 2 "Test2" []], [])) result

testExportStudents :: Test
testExportStudents = TestCase $ do
    let students = [Student 1 "Test Alice" " Smith" [101, 102], Student 2 "Test Bob" "Johnson" [103]]
    exportStudents students
    fileExists <- System.Directory.doesFileExist "exports/students_list.txt"
    assertEqual "Should export students" True fileExists
    removeFile "exports/students_list.txt"
    fileExistsAfterRemoval <- System.Directory.doesFileExist "exports/students_list.txt"
    assertEqual "Should remove students file after testing." False fileExistsAfterRemoval

testExportModules :: Test
testExportModules = TestCase $ do
    let modules = [Module 1 "Test1" [], Module 2 "Test2" []]
    exportModules modules
    fileExists <- System.Directory.doesFileExist "exports/modules_list.txt"
    assertEqual "Should export modules" True fileExists
    removeFile "exports/modules_list.txt"
    fileExistsAfterRemoval <- System.Directory.doesFileExist "exports/modules_list.txt"
    assertEqual "Should remove modules file after testing." False fileExistsAfterRemoval


testAddStudentToModule :: Test
testAddStudentToModule = TestCase $ do
    let students = [Student 1 "Test Alice" "Smith" [101, 102], Student 2 "Test Bob" "Johnson" [103]]
    let modules = [Module 101 "Test1" [1, 2], Module 102 "Test2" [1, 2], Module 103 "Test3" [2]]
    result <- addStudentToModule 1 103 students modules
    assertEqual "Should add a student to a module" (Right ([Student 1 "Test Alice" "Smith" [103, 101, 102], Student 2 "Test Bob" "Johnson" [103]], [Module 101 "Test1" [1, 2], Module 102 "Test2" [1, 2], Module 103 "Test3" [1, 2]])) result

testRemoveStudentFromModule :: Test
testRemoveStudentFromModule = TestCase $ do
    let students = [Student 1 "Test Alice" "Smith" [101, 102, 103], Student 2 "Test Bob" "Johnson" [103]]
    let modules = [Module 101 "Test1" [1, 2], Module 102 "Test2" [1, 2], Module 103 "Test3" [1, 2]]
    result <- removeStudentFromModule 1 103 students modules
    assertEqual "Should remove a student from a module" (Right ([Student 1 "Test Alice" "Smith" [101, 102], Student 2 "Test Bob" "Johnson" [103]], [Module 101 "Test1" [1, 2], Module 102 "Test2" [1, 2], Module 103 "Test3" [2]])) result

main :: IO ()
main = do
    _ <- runTestTT $ TestList tests
    return ()
