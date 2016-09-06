#import <CoreMIDI/CoreMIDI.h>
#import <CoreAudio/CoreAudio.h>
#import <pthread.h>


pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t read_condition_var = PTHREAD_COND_INITIALIZER;
pthread_cond_t write_condition_var = PTHREAD_COND_INITIALIZER;

int readtable = 0;
MIDIPacket* packet = NULL;
MIDIEndpointRef source = 0;


void midi_read_proc (MIDIPacketList* pktlist, void *readProcRefCon, void *srcConnRefCon) {

  pthread_mutex_lock(&mutex);

  packet = &pktlist->packet[0];
  source = (MIDIEndpointRef) srcConnRefCon;
  int num_packets = pktlist->numPackets;
  int i = 0;

  while (i < num_packets) {
    if (readtable == 0) {
      readtable++;
    }

    pthread_cond_signal(&read_condition_var);
    pthread_cond_wait(&write_condition_var, &mutex);

    if (readtable == 0) {
      packet = MIDIPacketNext(packet);
      i++;
    }
  }

  pthread_mutex_unlock(&mutex);
}

