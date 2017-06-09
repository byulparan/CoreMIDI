#import <CoreMIDI/CoreMIDI.h>

#import <pthread.h>


pthread_mutex_t incoming_packets = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t incoming_packet_ready = PTHREAD_COND_INITIALIZER;
pthread_cond_t incoming_packet_handled = PTHREAD_COND_INITIALIZER;

int incomingPacketFlag = 0;
MIDIPacket* incomingPacket = NULL;
MIDIEndpointRef incomingPacketEndpoint = 0;

void handleIncomingPackets
(MIDIPacketList *pktlist, void *readProcRefCon, void *srcConnRefCon)
{
  pthread_mutex_lock (&incoming_packets);

  int packetsNumber = pktlist->numPackets;

  incomingPacket = &pktlist->packet[0];
  incomingPacketEndpoint = (MIDIEndpointRef) srcConnRefCon;

  int i = 0;
  while (i < packetsNumber)
    {
      if (! incomingPacketFlag) incomingPacketFlag++;

      pthread_cond_signal (&incoming_packet_ready);
      pthread_cond_wait (&incoming_packet_handled, &incoming_packets);

      if (! incomingPacketFlag)
	{
	  incomingPacket = MIDIPacketNext (incomingPacket);
	  i++;
	}
    }

  pthread_mutex_unlock (&incoming_packets);
}
