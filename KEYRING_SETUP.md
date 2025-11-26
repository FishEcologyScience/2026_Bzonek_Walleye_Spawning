# Keyring Setup Instructions

## Initial Setup (Run Once)

The Google API key is now stored securely using the `keyring` package instead of being hardcoded in scripts.

### Step 1: Store Your API Key

Run this command **once** in R to store your Google API key:

```r
library(keyring)
keyring::key_set("google_api", username = "Jake")
```

You'll be prompted to enter the API key in a secure dialog box. Paste your Google API key when prompted.

### Step 2: Verify Storage

Test that the key is stored correctly:

```r
keyring::key_get("google_api", username = "Jake")
```

This should return your API key.

### Step 3: Run Your Scripts Normally

Your scripts will now automatically retrieve the API key from secure storage. No changes needed to your workflow!

## How It Works

- **Script1-1_format_data_h.R** now retrieves the API key using:
  ```r
  param_google_api_key <- keyring::key_get("google_api", username = "Jake")
  ggmap::register_google(key = param_google_api_key)
  ```

- The API key is stored in your system's credential manager (Windows Credential Manager)
- The key is never visible in your code or git repository
- The key persists across R sessions

## Security Benefits

✓ API key is encrypted in your system's credential store
✓ Never committed to git history
✓ Not visible in script files
✓ Secure storage following best practices

## Troubleshooting

If you get an error about the key not being found:
1. Make sure you've run `keyring::key_set()` at least once
2. Verify the username is exactly "Jake" (case-sensitive)
3. Check that keyring package is installed: `install.packages("keyring")`
