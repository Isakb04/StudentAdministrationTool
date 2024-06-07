# Student and Module Management System [Project Documentation]

## Overview
This Haskell program is designed to manage students and their enrollment in modules. It provides functionalities to add and remove students from modules, export student and module data to files, and more.

## Table of Contents

- [Project Description](#project-description)
- [Features](#features)
- [Setup Instructions](#setup-instructions)
- [Usage](#usage)
- [Testing](#testing)
- [Files Included](#files-included)
- [Versioning](#versioning)
- [License](#license)
- [Author](#author)
- [Acknowledgments](#acknowledgments)

## Project Description

The Student and Module Management System is a Haskell-based application that allows administrators to manage students and their enrollment in various modules. The program supports adding and removing students from modules, as well as exporting data to text files for easy record-keeping.

## Features

- find Students by first name, last name, or ID
- find Modules by name or ID
- Add students to a modules
- Remove students from a modules
- Export students and modules data to text files
- Add or remove students from modules
- Command-line for usability
- Defualt Non-interactive mode for quick commands
- Automated tests for key functionalities

## Setup Instructions

### Requirement

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

### Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/Isakb04/BS2220FunctionalProgramming.git
    cd student-module-management
    ```

2. Install dependencies and build the project:
    ```sh
    stack setup
    stack build
    ```
## Usage

To run the program, execute the following command in your terminal in the directory the Clone of the downloaded repository:

```sh
stack ghci BS2220FunctionalPogramming.hs
```
You will be presented with a command-line interface where you can perform various operations such as adding or removing students from modules, and exporting data.

## Commands

**Keep in minds:**

**Confirmation of modification to JSON**:
To finish a command at the  type “Y” or “y” to run the full command anything else will cancel, it does not ask for input so it's still non-interactive, more for saving accidental additions.
end

**toUpper (Head)**: all FirstName, LastName, modules and choosing the student or module which you want to work with will capitalize the first letter getting rid of errors in the searching and modification of JSON data.

**Example of toUpper**
```sh
Enter -> joe
It reads -> Joe
```

**Command Options**:

findStudent
```sh
findStudent <info>
```

findModule
```sh
findModule <info>
```

add
```sh
Student <firstName> <lastName> <modules> modules sepertaed by comma
Module <moduleName>
```

remove
```sh
Student <studentId>
Module <moduleId>
```

config
```sh
Add <studentId> <moduleId>
Remove <studentId> <moduleId>
```

export
```sh
exportStudents <Students>
exportModules <Modules>
```

commandSystem
```sh
now in Interface mode
[Command options]
1: Add Student or Module
2: Remove Student or Module
3: Find Student
4: Find Module
5: Export Data
0: Exit
Enter command number:
< >
```

Unless you type '0' and exit the interface mode commands finished will stay in the interface mode, when '0' is executed the default mode will be used as normal and the commands in 'help' will take over.

help
```sh
                   [Commands]
   commandSystem: Enter the command system
   add: addStudent <firstName> <lastName> <modules> sepertaed   by comma: Adds a new student.
   addModule <moduleName>: Adds a new module.
   remove: removeStudent <studentId>: Removes a student by ID.
   removeModule <moduleId>: Removes a module by ID.
   config: Add <studentId> <moduleId>: Adds a student to a module.
   Remove <studentId> <moduleId>: Removes a student from a module.
   findStudent: findStudent <info>: Finds a student by first name, last name, or ID.
   findModule: findModule <info>: Finds a module by name or ID.
   export: exportStudents <Students>: Exports the list of students to the specified file.
   exportModules <Modules>: Exports the list of modules to the specified file.
   :q : Exit
```

**Example Code**
```sh
Example Command
Add -> *Keypress ENTER*
Student -> *Keypress ENTER *
Joe -> *Keypress ENTER *
White -> *Keypress ENTER *
101,102,103 -> *Keypress ENTER *
Y -> *Keypress ENTER *
[Student added] *StudentId auto inclines*
```
Students.JSON now shows ->
```sh
    {
        "enrolledModules": [
            101,
            102,
            103
        ],
        "firstName": " Joe ",
        "lastName": " White ",
        "studentId": 1
    }
```

## Testing
The project includes automated tests to ensure the correctness of the implemented functionalities. To run the tests, use the following command:

```sh
stack test
```
The tests cover scenarios such as adding and removing students from modules and exporting data to files.

## Files Included
- `Main.hs`: Haskell file with core functionalities including data manipulation and IO operations.
- `Lib.hs`: Directory containing library files.
- `Spec.hs`: Directory containing specification files or test files.
- `Students.json`: JSON file containing initial data about students.
- `Modules.json`: JSON file containing initial data about modules.
- `README.md`: Markdown file containing information about the project and how to use it.
- `LICENSE`: File containing the license for the project.

## Versioning
This project uses file-based versioning for tracking changes in data files and source code.

## License
This project is licensed under the MIT License - see the LICENSE file for details.

## Author
- Isak Jonsson

## Acknowledgments
- Reuben Shaw
