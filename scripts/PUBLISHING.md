# Publishing to Hex

This document describes how to publish new versions of the grpc packages to Hex.pm.

## Package Structure

The project is organized as a monorepo with 3 packages:

1. **grpc_core** - Core types, codecs, and utilities (no dependencies on other grpc packages)
2. **grpc_server** - Server implementation (depends on grpc_core)
3. **grpc_client** - Client implementation (depends on grpc_core, and grpc_server for tests)

## Automated Release Process

The release process is automated via GitHub Actions when you push a git tag.

### Steps:

1. **Update versions** in all `mix.exs` files:
   - `grpc_core/mix.exs`
   - `grpc_server/mix.exs`
   - `grpc_client/mix.exs`

2. **Update CHANGELOG.md** with release notes

3. **Commit and push** changes:
   ```bash
   git add .
   git commit -m "Bump version to 1.0.0"
   git push
   ```

4. **Create and push a tag**:
   ```bash
   git tag v1.0.0
   git push origin v1.0.0
   ```

5. **GitHub Actions** will automatically:
   - Verify that all `mix.exs` versions match the tag (without the `v` prefix)
   - Publish packages in the correct order:
     1. grpc_core
     2. grpc_server (after grpc_core is available)
     3. grpc_client (after both are available)
   - Create a GitHub Release

## Manual Release Process

If you need to publish manually:

1. **Ensure you have Hex credentials configured**:
   ```bash
   mix hex.user auth
   ```

2. **Run the publish script**:
   ```bash
   ./scripts/publish_hex.sh 1.0.0
   ```

The script will:
- Verify versions in all mix.exs files
- Temporarily switch from path dependencies to hex dependencies
- Publish packages in order
- Restore path dependencies (for continued development)

## How It Works

### Dependency Management

During development, packages use **path dependencies**:
```elixir
{:grpc_core, path: "../grpc_core"}
```

For Hex publishing, these are temporarily switched to **hex dependencies**:
```elixir
{:grpc_core, "~> 1.0"}
```

The `publish_hex.sh` script handles this switching automatically and restores path dependencies after publishing.

### GitHub Secrets Required

The release workflow requires the following secret to be configured in GitHub:

- `HEX_API_KEY` - Your Hex.pm API key

To add this secret:
1. Go to GitHub repository → Settings → Secrets and variables → Actions
2. Click "New repository secret"
3. Name: `HEX_API_KEY`
4. Value: Your Hex API key (get it from `mix hex.user auth`)

## Troubleshooting

### Version mismatch error

If GitHub Actions fails with a version mismatch error, ensure all `@version` attributes in mix.exs files match the tag (without the `v` prefix).

Example for tag `v1.0.0`:
```elixir
@version "1.0.0"
```

### Package already exists

If a package version already exists on Hex, you cannot republish it. You must increment the version and create a new tag.

### Dependencies not found

If publishing fails because dependencies aren't found on Hex, ensure you're publishing in the correct order:
1. grpc_core (first, no dependencies)
2. grpc_server (depends on grpc_core)
3. grpc_client (depends on both)

The script handles this automatically by waiting between publishes.
