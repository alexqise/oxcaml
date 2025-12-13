// Import Firebase scripts inside the service worker
importScripts(
  "https://www.gstatic.com/firebasejs/12.6.0/firebase-app-compat.js"
);
importScripts(
  "https://www.gstatic.com/firebasejs/12.6.0/firebase-messaging-compat.js"
);

// Initialize Firebase here with the same config

const firebaseConfig = {
  apiKey: "AIzaSyAMNg5zUEAiwLU4TDTlLhGm3Pqp032se5Q",
  authDomain: "coop-slaythespire.firebaseapp.com",
  projectId: "coop-slaythespire",
  storageBucket: "coop-slaythespire.firebasestorage.app",
  messagingSenderId: "783879862672",
  appId: "1:783879862672:web:720391cc095eeed0f81b15",
  measurementId: "G-MYW8H985KZ",
};
firebase.initializeApp(firebaseConfig);

// Get messaging instance
const messaging = firebase.messaging();

// Handle background messages (when page is closed)
messaging.onBackgroundMessage(function (payload) {
  console.log(
    "[firebase-messaging-sw.js] Received background message ",
    payload
  );

  const notificationTitle = payload.notification?.title || "Game Notification";
  const notificationOptions = {
    body: payload.notification?.body || "You have a new update.",
    icon: "/assets/ironclad.png", // adjust to a real asset path
  };

  // Show the notification
  self.registration.showNotification(notificationTitle, notificationOptions);
});

// Optional: handle clicks on notifications
self.addEventListener("notificationclick", function (event) {
  event.notification.close();
  event.waitUntil(
    clients
      .matchAll({ type: "window", includeUncontrolled: true })
      .then((clientList) => {
        for (const client of clientList) {
          if (client.url.includes("/") && "focus" in client) {
            return client.focus();
          }
        }
        if (clients.openWindow) {
          return clients.openWindow("/"); // open your main page
        }
      })
      .catch((error) => {
        console.error("Notification click error:", error);
      })
  );
});
