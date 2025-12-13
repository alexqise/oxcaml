// Firebase Cloud Function (2nd Gen) for sending game notifications
// Automatically triggers when a new notification request is added to Firestore

const {onDocumentCreated} = require('firebase-functions/v2/firestore');
const {initializeApp} = require('firebase-admin/app');
const {getMessaging} = require('firebase-admin/messaging');
const {getFirestore, FieldValue} = require('firebase-admin/firestore');

// Initialize Firebase Admin
// No need for service account key - Cloud Functions have built-in credentials
initializeApp();

// Trigger when a new document is created in notificationRequests collection
exports.sendNotification = onDocumentCreated('notificationRequests/{docId}', async (event) => {
    const snap = event.data;
    if (!snap) {
      console.log('No data associated with the event');
      return;
    }
    const data = snap.data();
    const docId = event.params.docId;
    
    // Skip if already processed
    if (data.processed) {
      console.log('Skipping already processed notification:', docId);
      return null;
    }
    
    console.log('📬 New notification request:', docId);
    console.log('  User:', data.userId);
    console.log('  Game:', data.gameId);
    console.log('  Type:', data.type);
    console.log('  Token:', data.fcmToken?.substring(0, 20) + '...');
    
    try {
      // Build the notification message
      const message = {
        notification: {
          title: data.title || 'Game Notification',
          body: data.body || 'You have an update'
        },
        token: data.fcmToken,
        webpush: {
          fcmOptions: {
            // Link to your game - opens GitHub Pages when notification is clicked
            link: data.link || 'https://alexqise.github.io/oxcaml/'
          }
        }
      };
      
      // Send the notification using Firebase Cloud Messaging
      const response = await getMessaging().send(message);
      console.log('✅ Successfully sent notification!');
      console.log('  Response:', response);
      
      // Mark the notification request as processed
      await snap.ref.update({ 
        processed: true,
        processedAt: FieldValue.serverTimestamp(),
        response: response
      });
      
      return response;
      
    } catch (error) {
      console.error('❌ Error sending notification:', error.message);
      console.error('  Full error:', error);
      
      // Mark the notification request as failed
      await snap.ref.update({ 
        processed: true,
        failed: true,
        error: error.message,
        errorCode: error.code,
        processedAt: FieldValue.serverTimestamp()
      });
      
      // Re-throw the error so Firebase logs it
      throw error;
    }
  });
