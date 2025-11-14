# Testing Push Notifications

## Quick Start

### 1. Start the Notification Worker

The notification worker watches Firestore for notification requests and sends them automatically.

```bash
cd /Users/johnathanmo/oxcaml-2

# Install dependencies if needed
npm install firebase-admin

# Start the worker (keep this running in a separate terminal)
node notification-worker.js
```

You should see:

```
🚀 Notification worker started...
Watching for notification requests in Firestore...
```

### 2. Start Your Game

```bash
# In another terminal
cd /Users/johnathanmo/oxcaml-2
python3 -m http.server 8000
```

### 3. Test Turn Notifications

**Window 1 (Player 1):**

1. Open http://localhost:8000
2. Sign in with Google or email (e.g., player1@test.com)
3. Click "Create Lobby"
4. Copy the game ID

**Window 2 (Player 2):**

1. Open http://localhost:8000 in incognito mode
2. Sign in with different account (e.g., player2@test.com)
3. Paste game ID and click "Join Game"

**Play and Test:**

1. Window 1: Play a card, click "End Turn"
2. Check notification-worker terminal - you should see:
   ```
   📬 New notification request: ...
   ✅ Successfully sent notification!
   ```
3. Window 2: Should receive notification "Your Turn! 🎮"
4. Window 2: Close the tab completely, then have Player 1 end turn again
5. You should see a system notification even with tab closed!

### 4. Check Firestore (Optional)

Go to Firebase Console → Firestore Database

You should see:

- `users/{user_id}` - Contains `fcmToken` for each user
- `notificationRequests` - Contains queued/processed notifications
- `scheduledNotifications` - Contains daily reminders

### What to Look For

**In notification-worker.js terminal:**

- 📬 New notification request appears
- ✅ Successfully sent message
- Response ID from FCM

**In browser console:**

- "FCM token registered: ..."
- "Turn notification queued for user: ..."
- "Message received in foreground: ..." (if tab is open)

**In browser:**

- Notification popup with "Your Turn! 🎮"
- Clicking notification opens/focuses your game

## Debugging

### No notification received?

1. **Check FCM tokens are saved:**

   - Firebase Console → Firestore → `users/{user_id}`
   - Should have `fcmToken` field

2. **Check notification was queued:**

   - Firestore → `notificationRequests`
   - Should see new documents when turns end

3. **Check notification-worker.js is running:**

   - Should see "Watching for notification requests..."
   - Should show new requests as they come in

4. **Check browser permission:**

   ```javascript
   // In browser console
   Notification.permission; // Should be "granted"
   ```

5. **Check service worker:**
   ```javascript
   // In browser console
   navigator.serviceWorker
     .getRegistration()
     .then((reg) => console.log("SW:", reg));
   ```

### Manual Test

Send a test notification directly:

```bash
# Edit send-notification.js with your FCM token
# Then run:
node send-notification.js
```

## How It Works

1. **Sign In** → Browser gets FCM token → Saved to Firestore
2. **End Turn** → OCaml creates notification request in Firestore
3. **Worker** → Watches Firestore → Sends notification via FCM API
4. **Browser** → Receives push → Shows notification

## Production Deployment

For production, deploy `notification-worker.js` as:

- **Firebase Cloud Function** (recommended)
- **Google Cloud Run**
- **Any Node.js server**

The worker will run 24/7 and process notifications automatically.
