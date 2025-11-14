// Simple script to send push notifications via Firebase Cloud Messaging
const admin = require("firebase-admin");

// Load the service account key (download from Firebase Console)
const serviceAccount = require("./service-account-key.json");

// Initialize Firebase Admin SDK
admin.initializeApp({
  credential: admin.credential.cert(serviceAccount),
});

// REPLACE THIS with your actual FCM token from the browser console
const FCM_TOKEN =
  "ec6QFVGwLRi2nMIttxrlFE:APA91bFdwWIqk8MuqpK9cjPVi5PEk16QBSSLL0CX2muR1nzE3icKbvc8gUyV-h5A5zgVYZKpwsxmOd_nZUpYN4Ou1oTyAU-pV8RhIqkKgZowiUtdeNvQE7s";

// The notification message
const message = {
  notification: {
    title: "Test Notification asdfasdf 🎮",
    body: "Your push notifications are working!",
  },
  token: FCM_TOKEN,
  // Optional: Opens your game when notification is clicked
  webpush: {
    fcmOptions: {
      link: "http://localhost:8000",
    },
  },
};

// Send the notification
admin
  .messaging()
  .send(message)
  .then((response) => {
    console.log("✅ Successfully sent notification!");
    console.log("Response:", response);
    process.exit(0);
  })
  .catch((error) => {
    console.error("❌ Error sending notification:", error);
    process.exit(1);
  });
