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
    icon: "./assets/ironclad.png", // use relative path for GitHub Pages subdirectory
  };

  // Show the notification
  self.registration.showNotification(notificationTitle, notificationOptions);
});

// Handle notification clicks - open the game
self.addEventListener("notificationclick", function (event) {
  event.notification.close();
  
  // Get the URL from the notification data, or default to the game home
  // Using relative path for GitHub Pages subdirectory compatibility
  const urlToOpen = event.notification.data?.url || self.location.origin + '/oxcaml/';
  
  console.log('[SW] Notification clicked, opening URL:', urlToOpen);
  
  event.waitUntil(
    clients
      .matchAll({ type: "window", includeUncontrolled: true })
      .then((clientList) => {
        // Check if game is already open in a tab
        for (const client of clientList) {
          if (client.url.includes('/oxcaml/') && 'focus' in client) {
            // Found an open tab with the game, focus it
            console.log('[SW] Found existing game tab, focusing it');
            return client.focus();
          }
        }
        // No existing tab, open a new one
        console.log('[SW] No existing game tab, opening new window');
        if (clients.openWindow) {
          return clients.openWindow(urlToOpen);
        }
      })
      .catch((error) => {
        console.error("Notification click error:", error);
      })
  );
});
