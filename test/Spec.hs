import Test.HUnit
import Lib


-- Main function to run all tests
main :: IO Counts
main = runTestTT $ TestList [
    testLoadStudents,
    testLoadModules,
    testFindStudentByFirstName,
    testFindModuleByName,
    testAddStudent,
    testRemoveStudent,
    testExportStudents,
    testExportModules,
    testUpdateStudent
    ]

-- Test for loading students
testLoadStudents :: Test
testLoadStudents = TestCase $ do
    result <- loadStudents
    assertEqual "Should load students correctly" (Right [Student 1 "John" "Doe" [101, 102]]) result

-- Test for loading modules
testLoadModules :: Test
testLoadModules = TestCase $ do
    result <- loadModules
    assertEqual "Should load modules correctly" (Right [Module 101 "Mathematics" 10, Module 102 "Physics" 8]) result

-- Test for finding students by first name
testFindStudentByFirstName :: Test
testFindStudentByFirstName = TestCase $ do
    let students = Right [Student 1 "John" "Doe" [101], Student 2 "Jane" "Doe" [102]]
    let result = findStudentByFirstName "John" students
    assertEqual "Should find students by first name" (Right [Student 1 "John" "Doe" [101]]) result

-- Test for finding modules by name
testFindModuleByName :: Test
testFindModuleByName = TestCase $ do
    let modules = Right [Module 101 "Mathematics" 10, Module 102 "Physics" 8]
    let result = findModuleByName "Physics" modules
    assertEqual "Should find modules by name" (Right [Module 102 "Physics" 8]) result

-- Test for adding a new student
testAddStudent :: Test
testAddStudent = TestCase $ do
    let students = [Student 1 "John" "Doe" [101]]
    let newStudent = Student 2 "Jane" "Doe" [102]
    let result = addStudent newStudent students
    assertBool "Should contain the new student" (newStudent `elem` result)

-- Test for removing a student
testRemoveStudent :: Test
testRemoveStudent = TestCase $ do
    let students = [Student 1 "John" "Doe" [101], Student 2 "Jane" "Doe" [102]]
    let result = removeStudent 1 students
    assertEqual "Should remove the student correctly" [Student 2 "Jane" "Doe" [102]] result

-- Test for exporting students
testExportStudents :: Test
testExportStudents = TestCase $ do
    let students = [Student 1 "John" "Doe" [101]]
    result <- exportStudents students
    assertEqual "Should export data correctly" "Export successful" result

testExportModules :: Test
testExportModules = TestCase $ do
    let modules = [Module 101 "Mathematics" 10, Module 102 "Physics" 8]
    result <- exportModules modules
    assertEqual "Should export data correctly" "Export successful" result

-- Test for updating a student's details
testUpdateStudent :: Test
testUpdateStudent = TestCase $ do
    let students = [Student 1 "John" "Doe" [101], Student 2 "Jane" "Doe" [102]]
    let updatedStudent = Student 1 "John" "Smith" [101, 103]
    let result = updateStudent 1 updatedStudent students
    assertEqual "Should update the student correctly" [updatedStudent, Student 2 "Jane" "Doe" [102]] result