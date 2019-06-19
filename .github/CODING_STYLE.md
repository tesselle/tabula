# Coding Style Guide

## General Rules

* Use `<-` for assignment, NOT `=`.
```{r}
# Good
x <- 1

# Bad
x = 1
1 -> 1
```
* In a function call, specify arguments by name. Never specify by partial name and never mix by position and complete name.
```{r}
# Good
mean(x, na.rm = TRUE)

# Bad
mean(x, na = TRUE)
1 -> 1
```
* The required arguments should be first, followed by optional arguments.
* The `...` argument should either be in the beginning or in the end.
* Always validate arguments in a function.
* Specify the namespace of each used function, except if it is from `base` package.
* Do NOT put more than one statement per line. Do NOT use semicolon as termination of the command.

## Naming
As a general rule, abbreviations must be avoided when naming.

### Naming Files

* File names must use `.R` extension.
* File names must be meaningful.
* File names must be lowercase.
* File names must not contain `/` and spaces. Instead, a dash (`-`) or underscore (`_`) should be used.
* File names must use letters from Basic Latin, and NOT from Latin-1 Supplement.
* Use `-methods` suffix for S4 class methods.
* Mind special names:
    * `AllClasses.R` stores all S4 classes definitions.
    * `AllGenerics.R` stores all S4 generic functions.
    * `zzz.R` contains `.onLoad()` and friends.

### Naming Variables

* Variables names must be as short as possible.
* Variables names must be meaningful nouns.
* Variable names must be lowercase.
* Never separate words within the name by `.` (reserved for an S3 dispatch) or use PascalCase (reserved for S4 classes definitions). Instead, use an underscore (`_`).
* Do not use names of existing function and variables (especially, built-in ones).

### Naming Functions

* Function names must start with a verb.
* Function names must be in camelCase: the first letter of an identifier is lowercase and the first letter of each subsequent concatenated word is capitalized.
* Use `.` only for dispatching S3 generic.

### Naming S4 Classes

* Class names must be nouns in PascalCase with initial capital case letter and the first letter of each subsequent concatenated word capitalized.

## Syntax
### Line Length

* The maximum length of lines is limited to 80 characters.

### Spacing

* Put spaces around all infix binary operators (`=`, `+`, `*`, `==`, `&&`, `<-`, `%*%`, etc.).
```{r}
# Good 
x == y
z <- 2 + 1

# Bad
x==y
z<-2+1
```
* Put spaces around `=` in function calls.
* Do not place space for subsetting (`$` and `@`), namespace manipulation (`::` and `:::`), and for sequence generation (`:`).
* Put a space after a comma.
* Use a space before left parentheses, except in a function call.
* No spacing around code in parenthesis or square brackets.

### Curly Braces

* An opening curly brace should never go on its own line and should always be followed by a new line.
* A closing curly brace should always go on its own line, unless it's followed by `else`.
* Always indent the code inside curly braces.
* Curly braces and new lines can be avoided, if a statement after `if` is very short.

### Indentation

* Do not use tabs or mixes of tabs and spaces for indentation.
* Use two spaces for indentation.

### New Line

* In a function definition or call excessive arguments must be indented where the closing parenthesis is located, if only two lines are sufficient.
* Otherwise, each argument can go into a separate line, starting with a new line after the opening parenthesis.
* If the condition in `if` statement expands into several lines, than each condition must end with a logical operator, NOT start with it.
* If the statement, which contains operators, expands into several lines, then each line should end with an operator and not begin with it.
* Each grammar statement of `dplyr` (after `%>%`) and `ggplot2` (after `+`) should start with a new line.

### Comments

* Comments start with `#` followed by space and text of the comment.
* Comments should explain the why, not the what. Comments should explain the overall intention of the command.
* Short comments can be placed on the same line of the code.
* It makes sense to split the source into logical chunks by `#` followed by `-` or `=`.
