<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- SPDX-FileCopyrightText: 2024-2025 hyperpolymath -->

# GitHub App Setup Guide

This guide explains how to create and configure a GitHub App for Oikos Bot.

## Prerequisites

- GitHub account with organization access (or personal account)
- OpenSSL for private key conversion

## Step 1: Create the GitHub App

1. Go to **Settings** > **Developer settings** > **GitHub Apps**
2. Click **New GitHub App**
3. Fill in the details:

   | Field | Value |
   |-------|-------|
   | **GitHub App name** | Oikos Bot (or your chosen name) |
   | **Homepage URL** | Your repo URL |
   | **Webhook URL** | `https://your-domain.com/webhooks/github` |
   | **Webhook secret** | Generate a secure random string |

4. Set **Permissions**:

   | Permission | Access |
   |------------|--------|
   | Contents | Read |
   | Pull requests | Read and write |
   | Checks | Read and write (optional) |
   | Metadata | Read |

5. Subscribe to **Events**:
   - Pull request
   - Push (optional)

6. Under **Where can this GitHub App be installed?**
   - Select "Only on this account" for private use
   - Select "Any account" for public marketplace

7. Click **Create GitHub App**

## Step 2: Generate and Convert Private Key

1. On the App settings page, scroll to **Private keys**
2. Click **Generate a private key**
3. Save the `.pem` file securely

GitHub provides PKCS#1 format keys. Convert to PKCS#8 for Web Crypto API:

```bash
openssl pkcs8 -topk8 -inform pem -in your-app-key.pem -outform pem -nocrypt -out your-app-key-pkcs8.pem
```

## Step 3: Get Your App ID

On the App settings page, note the **App ID** (a number like `123456`).

## Step 4: Configure Environment Variables

Set the following environment variables:

```bash
# Required for GitHub App authentication
export GITHUB_APP_ID="123456"
export GITHUB_PRIVATE_KEY="$(cat your-app-key-pkcs8.pem)"

# Required for webhook signature verification
export GITHUB_WEBHOOK_SECRET="your-webhook-secret"

# Optional
export BOT_MODE="advisor"  # advisor, consultant, or regulator
export PORT="3000"
```

For production, use a secrets manager. For Kubernetes:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: oikos-bot
type: Opaque
stringData:
  GITHUB_APP_ID: "123456"
  GITHUB_PRIVATE_KEY: |
    -----BEGIN PRIVATE KEY-----
    ...
    -----END PRIVATE KEY-----
  GITHUB_WEBHOOK_SECRET: "your-webhook-secret"
```

## Step 5: Install the App

1. On the App settings page, click **Install App** in the sidebar
2. Select the account/organization
3. Choose repositories:
   - All repositories, or
   - Only select repositories
4. Click **Install**

## Step 6: Verify Installation

After installing, check the server logs for:

```
{"level":"info","message":"Posted PR comment","data":{"pr":1,"commentId":12345}}
```

Create a test PR to verify the bot posts analysis comments.

## Troubleshooting

### "GitHub App credentials not configured"

- Verify `GITHUB_APP_ID` and `GITHUB_PRIVATE_KEY` are set
- Check private key is in PKCS#8 format (starts with `-----BEGIN PRIVATE KEY-----`)

### "No installation ID in payload"

- Ensure the app is installed on the repository
- Check webhook is reaching the server

### "GitHub API error 401"

- Verify App ID is correct
- Check private key matches the App
- Ensure private key is not expired

### JWT Signature Issues

If you see signing errors:

```bash
# Verify key format
openssl rsa -in your-key.pem -check

# Re-convert to PKCS#8
openssl pkcs8 -topk8 -inform pem -in your-key.pem -outform pem -nocrypt -out your-key-pkcs8.pem
```

## API Rate Limits

GitHub App installation tokens have higher rate limits than personal access tokens:

- 5,000 requests per hour per installation
- Rate limit resets hourly

The bot caches installation tokens (valid for 1 hour) to minimize token exchange requests.

## Security Notes

- Never commit private keys to version control
- Use environment variables or secrets managers
- Rotate webhook secrets periodically
- Monitor App audit logs for unusual activity
