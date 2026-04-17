# Agent Guidelines for This Repository

## Project Overview

This is an IHP (Integrated Haskell Platform) web application using:
- **Framework**: IHP master latest (Haskell web framework)
- **Build System**: Nix + devenv for reproducible development
- **Database**: PostgreSQL (schema in `Application/Schema.sql`)

## Build, Test & Lint Commands

```bash
# Development server is already running via 'devenv up' - do NOT try to start it yourself
# The server auto-recompiles on file changes

# Build generated files (required after schema changes)
make build/Generated/Types.hs

# Linting & Formatting
stylish-haskell -i **/*.hs   # Format all Haskell files in-place (always run this before committing)

# To run tests:
runghc $(make print-ghc-extensions) -i. -ibuild -iConfig Test/Main.hs
```

## Important Notes

- **Never try to build or run the app manually** - devenv is always running in the background with hot reload
- If you need to check for compile errors, ask the user to run `devenv up` and show you the output
- The dev server automatically recompiles and reloads when files change

## Code Style Guidelines

### Imports
- Use `IHP.Prelude` (not standard Prelude)
- Import project modules fully qualified or explicit
- Example:
  ```haskell
  import IHP.Prelude
  import Generated.Types
  import IHP.ControllerPrelude
  import Web.Routes
  import Web.Types
  ```

### Formatting
- Run `stylish-haskell` before committing
- 80 column line limit
- 4-space indentation
- No trailing whitespace
- Vertical language pragmas (one per line)

### Naming Conventions
- **Modules**: PascalCase (e.g., `Web.Controller.Users`)
- **Types**: PascalCase (e.g., `UserId`, `MarketAction`)
- **Functions**: camelCase (e.g., `fetchRecords`, `setSuccessMessage`)
- **Record fields**: camelCase with type prefix (e.g., `userId`, `marketName`)
- **Controllers**: Suffix with `Controller` (e.g., `UsersController`)
- **Actions**: Suffix with `Action` (e.g., `ShowUserAction`)

### Type Signatures
- Always provide explicit type signatures for top-level functions
- Use IHP's type aliases: `Id User`, `Text`, `Int64`
- Leverage `?context` and `?modelContext` implicit parameters

### Error Handling
- Use IHP's `catch` and `catchAll` for exceptions
- Use `setErrorMessage` for user-facing errors
- Use `accessDeniedUnless` for authorization checks
- Validation via `validate` and `validateField` helpers

### Database Operations
- Use IHP's query DSL with `|>`:
  ```haskell
  users <- query @User
      |> filterWhere (#isActive, True)
      |> orderBy #createdAt
      |> fetch
  ```
- Use `fetch` for single records, `fetchCount` for counts
- Use `createRecord`, `updateRecord`, `deleteRecord` for mutations

## Project Structure

```
Application/
  Schema.sql          # Database schema
  Helper/
    Controller.hs     # Controller helpers
    View.hs           # View helpers
    LMSR.hs           # Math helpers for prediction markets

Config/
  Config.hs           # App configuration

Web/
  Controller/         # HTTP controllers
  View/               # HSX templates
  Job/                # Jobs modules
  Types.hs            # Web-specific types
  Routes.hs           # URL routing

build/Generated/      # Auto-generated from schema
Main.hs               # Application entry
flake.nix             # Nix development environment
```

## Hoogle
When you need to find functions, look up type signatures, discover data structures, or read documentation for any Haskell package used in this project, use Hoogle. It indexes all packages from `flake.nix`, so it's the primary way to explore available APIs and read Hackage-style documentation without leaving the dev environment.
```bash
hoogle search "Text -> ByteString"   # Search by type signature, function name, or data type
# Hoogle web UI at http://localhost:8002 - browse and read full Hackage docs for all project packages
```

## Common Tasks

**Add a new controller**: Create in `Web/Controller/`, add route in `Web/Routes.hs`, create views in `Web/View/`

**Modify schema**: Edit `Application/Schema.sql`, run `make build/Generated/Types.hs`

**Add a helper**: Create in `Application/Helper/`, export from `Application/Helper/Controller.hs` or `View.hs`
