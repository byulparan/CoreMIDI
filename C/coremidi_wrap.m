#import <CoreMIDI/CoreMIDI.h>
#import <CoreAudio/CoreAudio.h>
#import <pthread.h>

int current_index = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t condition_var = PTHREAD_COND_INITIALIZER;

const int PACKETS_SIZE = 256;
static MIDIPacket packets[PACKETS_SIZE];
static MIDIEndpointRef sources[PACKETS_SIZE];


MIDIEndpointRef get_packet (MIDIPacket** packet, int index) {
  *packet = packets + index;
  return sources[index];
}

void midi_read_proc (MIDIPacketList* pktlist, void *readProcRefCon, void *srcConnRefCon) {
  pthread_mutex_lock(&mutex);
  MIDIPacket *packet = &pktlist->packet[0];
  for(int i = 0; i < pktlist->numPackets; i++) {
    memcpy(packets+current_index, packet, sizeof(MIDIPacket));
    sources[current_index] = (MIDIEndpointRef) srcConnRefCon;
    current_index = (current_index + 1) % PACKETS_SIZE;
    packet = MIDIPacketNext(packet);
  }
  pthread_cond_signal(&condition_var);
  pthread_mutex_unlock(&mutex);
}

