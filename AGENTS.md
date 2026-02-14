# Agent Guidelines for This Repository

## Project Overview

This is an IHP (Integrated Haskell Platform) web application using:
- **Framework**: IHP v1.4.1 (Haskell web framework)
- **Build System**: Nix + devenv for reproducible development
- **Database**: PostgreSQL (schema in `Application/Schema.sql`)
- **Deployment**: NixOS with `deploy-to-nixos`

## Build, Test & Lint Commands

```bash
# Development server
# devenv up                    # Start PostgreSQL + IHP dev server
# devenv is always running for you with autorefresh

# Build generated files (required after schema changes)
make build/Generated/Types.hs

# Linting & Formatting
stylish-haskell -i **/*.hs   # Format all Haskell files in-place (always run this before committing)

# To run tests when they exist:
runghc $(make print-ghc-extensions) -i. -ibuild -iConfig Test/Main.hs
```

## Code Style Guidelines

### Imports
- Import order: Prelude/IHP first, then generated types, then project modules
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
  Fixtures.sql        # Test fixtures
  Helper/
    Controller.hs     # Controller helpers
    View.hs           # View helpers
    LMSR.hs           # Math helpers for prediction markets

Config/
  Config.hs           # App configuration

Web/
  Controller/         # HTTP controllers
  View/               # HSX templates
  Types.hs            # Web-specific types
  Types/              # Domain types (Money, etc.)
  Routes.hs           # URL routing

Admin/
  Controller/         # Admin controllers
  View/               # Admin views
  Types.hs            # Admin types

build/Generated/      # Auto-generated from schema
Main.hs               # Application entry
flake.nix             # Nix development environment
```

## Common Tasks

**Add a new controller**: Create in `Web/Controller/`, add route in `Web/Routes.hs`, create views in `Web/View/`

**Modify schema**: Edit `Application/Schema.sql`, run `make build/Generated/Types.hs`

**Add a helper**: Create in `Application/Helper/`, export from `Application/Helper/Controller.hs` or `View.hs`

## CI/CD

GitHub Actions runs tests on push/PR to main. Deploys to NixOS after successful tests.

Secrets required: `SSH_HOST`, `SSH_USER`, `SSH_PRIVATE_KEY`
