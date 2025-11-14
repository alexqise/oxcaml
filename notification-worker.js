// Simple notification worker that watches Firestore for notification requests
// and sends them using Firebase Admin SDK
const admin = require('firebase-admin');

// Load the service account key
const serviceAccount = require('./service-account-key.json');

// Initialize Firebase Admin SDK
admin.initializeApp({
  credential: admin.credential.cert(serviceAccount)
});

const db = admin.firestore();

console.log('🚀 Notification worker started...');
console.log('Watching for notification requests in Firestore...\n');

// Watch the notificationRequests collection for new documents
const unsubscribe = db.collection('notificationRequests')
  .where('processed', '==', false)
  .onSnapshot(async (snapshot) => {
    snapshot.docChanges().forEach(async (change) => {
      if (change.type === 'added') {
        const doc = change.doc;
        const data = doc.data();
        
        console.log('\n📬 New notification request:', doc.id);
        console.log('  User:', data.userId);
        console.log('  Game:', data.gameId);
        console.log('  Type:', data.type);
        console.log('  Token:', data.fcmToken?.substring(0, 20) + '...');
        
        // Send the notification
        try {
          const message = {
            notification: {
              title: data.title || 'Game Notification',
              body: data.body || 'You have an update'
            },
            token: data.fcmToken,
            webpush: {
              fcmOptions: {
                link: 'http://localhost:8000'  // Opens your game when clicked
              }
            }
          };
          
          const response = await admin.messaging().send(message);
          console.log('  ✅ Successfully sent notification!');
          console.log('  Response:', response);
          
          // Mark as processed
          await doc.ref.update({ 
            processed: true,
            processedAt: admin.firestore.FieldValue.serverTimestamp(),
            response: response
          });
          
        } catch (error) {
          console.error('  ❌ Error sending notification:', error.message);
          
          // Mark as failed
          await doc.ref.update({ 
            processed: true,
            failed: true,
            error: error.message,
            processedAt: admin.firestore.FieldValue.serverTimestamp()
          });
        }
      }
    });
  }, (error) => {
    console.error('Error watching collection:', error);
  });

// Handle graceful shutdown
process.on('SIGINT', () => {
  console.log('\n\n👋 Shutting down notification worker...');
  unsubscribe();
  process.exit(0);
});

console.log('Press Ctrl+C to stop\n');

