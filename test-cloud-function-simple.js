// Simple test to verify the cloud function triggers (even if sending fails)
// This creates a notification request with a fake token
// The function will trigger, try to send, and mark as failed

const admin = require('firebase-admin');

// Load the service account key
const serviceAccount = require('./service-account-key.json');

// Initialize Firebase Admin SDK
admin.initializeApp({
  credential: admin.credential.cert(serviceAccount)
});

const db = admin.firestore();

console.log('🧪 Simple Cloud Function Test (with fake token)...\n');

async function testCloudFunction() {
  try {
    // Create a notification request with a fake token
    // The function will trigger and mark it as failed (expected behavior)
    const docRef = await db.collection('notificationRequests').add({
      userId: 'test-user-123',
      gameId: 'test-game-456',
      type: 'test',
      title: '🧪 Cloud Function Test',
      body: 'Testing if the cloud function triggers correctly',
      fcmToken: 'fake-token-for-testing',
      processed: false,
      createdAt: admin.firestore.FieldValue.serverTimestamp()
    });
    
    console.log('✅ Created test notification request:', docRef.id);
    console.log('⏳ Waiting for cloud function to process...\n');
    
    // Watch for updates from the cloud function
    const unsubscribe = docRef.onSnapshot((doc) => {
      const data = doc.data();
      
      if (data.processed) {
        console.log('✅ Cloud function triggered and processed the request!');
        console.log('   Document ID:', doc.id);
        console.log('   Processed at:', data.processedAt?.toDate());
        
        if (data.failed) {
          console.log('   Status: Failed (expected with fake token)');
          console.log('   Error:', data.error);
          console.log('\n✅ SUCCESS: Cloud function is working correctly!');
          console.log('   It triggered, attempted to send, and logged the error.\n');
        } else {
          console.log('   Status: Success (unexpected with fake token)');
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
      console.log('\n⏱️  Timeout: Cloud function did not respond in 30 seconds');
      console.log('\n   Check function logs:');
      console.log('   npx firebase-tools functions:log\n');
      unsubscribe();
      process.exit(1);
    }, 30000);
    
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

testCloudFunction();


