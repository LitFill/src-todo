# src-todo

CLI tool for managing TODO comments in source code files.

## Description

`src-todo` is a command-line interface written in Haskell (btw) that scans
source files for TODO comments, allowing users to register new TODOs with
unique IDs, list existing TODOs, display details for a specific TODO by ID, and
replace TODO IDs. It supports TODO formats like "TODO: (id) description" or
"TODO: description", where IDs are optional and generated as timestamp-based
strings (first 20 characters of YYYYmmDDHHMMSSqqqqqq format).

The tool operates on files or directories (default: current directory), parsing
comments using Megaparsec and handling file updates in place.

## Installation

1. Ensure you have Haskell installed (GHC recommended) along with Cabal or Stack.

2. Clone the repository:

   ```sh
   git clone https://github.com/LitFill/src-todo.git
   ```

3. Navigate to the project directory:

   ```sh
   cd src-todo
   ```

4. Build and install the executable:
   - Using Cabal:

     ```sh
     cabal build
     cabal install
     ```

   - Using Stack:

     ```sh
     stack build
     stack install
     ```

This will make the `src-todo` command available in your PATH.

## Usage

Run the tool with:

```sh
src-todo <command> [options] [FILES...]
```

If no files are specified, it defaults to the current directory (`.`).

### Commands

- **register**: Scans for TODOs without IDs, assigns a new timestamp-based ID,
  updates the files in place, and outputs the new IDs.

  ```sh
  src-todo register [FILES...]
  ```

  Example:

  ```sh
  src-todo register src/
  ```

  Output (if new TODOs are found):

  ```
  Registered new todos with these ids:
  20250914123456789012
  ```

- **show <ID>**: Displays details for a TODO with the specified ID, including
  note, ID, and location (file:line).

  ```sh
  src-todo show <ID> [FILES...]
  ```

  Example:

  ```sh
  src-todo show 20250914123456789012 src/
  ```

  Output:

  ```
  # Todo
    - note     : Implement feature X
    - id       : 20250914123456789012
    - location : src/module.hs:10
  ```

- **list**: Lists all TODOs found in the specified files/directories,
  displaying each with note, ID (if present), and location.

  ```sh
  src-todo list [FILES...]
  ```

  Example:

  ```sh
  src-todo list .
  ```

  Output:

  ```
  # Todo
    - note     : Fix bug Y
    - id       : 20250914123456789012
    - location : src/file.hs:5

  # Todo
    - note     : Optimize Z
    - id       :
    - location : src/another.hs:20
  ```

- **replace-id <OLD_ID> <NEW_ID>**: Replaces the specified old ID with a new ID
  in matching TODOs and updates the files.

  ```sh
  src-todo replace-id <OLD_ID> <NEW_ID> [FILES...]
  ```

  Example:

  ```sh
  src-todo replace-id 20250914123456789012 new-custom-id src/
  ```

  Output:

  ```
  The id 20250914123456789012 is replaced with new-custom-id
  ```

## Contributing

Contributions are welcome. Fork the repository, make changes, and submit a pull
request. Ensure code adheres to my Haskell best practices.

## License

MIT License. See [LICENSE](LICENSE) file for details.


