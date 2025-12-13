// Test script to verify the cloud function works
// This creates a test notification request in Firestore
// The cloud function should automatically trigger and send it

const admin = require('firebase-admin');

// Load the service account key
const serviceAccount = require('./service-account-key.json');

// Initialize Firebase Admin SDK
admin.initializeApp({
  credential: admin.credential.cert(serviceAccount)
});

const db = admin.firestore();

console.log('🧪 Testing Cloud Function...\n');

// Create a test notification request
async function testCloudFunction() {
  try {
    // You need to replace this with a real FCM token from your device
    // To get your token, check your browser console when you load the game
    const TEST_FCM_TOKEN = 'YOUR_FCM_TOKEN_HERE';
    
    if (TEST_FCM_TOKEN === 'YOUR_FCM_TOKEN_HERE') {
      console.log('❌ Error: You need to set a real FCM token!');
      console.log('\nTo get your FCM token:');
      console.log('1. Open your game in the browser');
      console.log('2. Open browser DevTools console (F12)');
      console.log('3. Look for "FCM Token:" in the logs');
      console.log('4. Copy the token and paste it in this script\n');
      process.exit(1);
    }
    
    // Create a notification request document
    const docRef = await db.collection('notificationRequests').add({
      userId: 'test-user-123',
      gameId: 'test-game-456',
      type: 'your_turn',
      title: '🎮 Test Notification',
      body: 'This is a test notification from the cloud function!',
      fcmToken: TEST_FCM_TOKEN,
      processed: false,
      createdAt: admin.firestore.FieldValue.serverTimestamp()
    });
    
    console.log('✅ Created test notification request:', docRef.id);
    console.log('⏳ Waiting for cloud function to process...\n');
    
    // Watch for the document to be updated by the cloud function
    const unsubscribe = docRef.onSnapshot((doc) => {
      const data = doc.data();
      
      if (data.processed) {
        console.log('✅ Cloud function processed the notification!');
        console.log('   Document ID:', doc.id);
        console.log('   Processed at:', data.processedAt?.toDate());
        
        if (data.failed) {
          console.log('   ❌ Status: FAILED');
          console.log('   Error:', data.error);
          console.log('   Error Code:', data.errorCode);
        } else {
          console.log('   ✅ Status: SUCCESS');
          console.log('   Response:', data.response);
          console.log('\n📱 Check your device for the notification!');
        }
        
        unsubscribe();
        process.exit(0);
      }
    }, (error) => {
      console.error('❌ Error watching document:', error);
      unsubscribe();
      process.exit(1);
    });
    
    // Timeout after 30 seconds
    setTimeout(() => {
      console.log('\n⏱️  Timeout: Cloud function did not process in 30 seconds');
      console.log('   This might mean:');
      console.log('   - The function is still warming up (first run can be slow)');
      console.log('   - There was an error in the function');
      console.log('   - Check logs with: npx firebase-tools functions:log\n');
      unsubscribe();
      process.exit(1);
    }, 30000);
    
  } catch (error) {
    console.error('❌ Error creating test notification:', error);
    process.exit(1);
  }
}

testCloudFunction();


