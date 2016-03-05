Lightning payments over a N step path requires (N*4)-1 network messages.

Here's an example where N=2.

channel_manger is the channel state that your partner signed over.
channel_partner is the channel state that you signed over.
arbitrage is some state that the server keeps track of. It lists 2 channels, and the hash of a bet that they share in common.

If I want to pay you through a server:

1) I tell the server I want to send the payment, and propose new channel state. And I give the server the secret encrypted so only you can read it.
*I update channel_partner
*server updates channel_manager
*server updates arbitrage

2) the server finishes updating my channel by sending me a signature.
*server updates channel_partner
*I update channel_manager

3) the server tells you about the payment, and starts updating your channel. The server gives you the encrypted secret.
*server updates channel_partner
*uses mailbox
*you update channel_manager

4) you finish updating the channel by sending a signature. And you unlock the funds by sending the secret to the server
*you update channel_partner
*server updates channel_manager

5) the server acknowledges that the secret is valid by sending you a signature, which finishes updating your channel.
*you update channel_manager
*server updates channel_partner

6) the server sends you the secret and a signature to unlock my payment.
*uses mailbox
*server updates channel_partner
*I update channel_manager

7) I acknowledge that the secret is valid by sending a signature to the server.
*server updates arbitrage
*server updates channel_manager
*I update channel_partner


For a bet, both parties need to put some money at stake. This can be done trustlessly by adding a hashlock to the bet. Until the hashlock is removed, each party would get their own money back. Once the hashlock is removed, the bet becomes active.

1) I tell the server I want to add money to the bet. I update my channel state. I send a signature and new channel state to the server.
*I update channel_partner
*server updates channel_manager
*server updates arbitrage

2) The server responds by updating the channel state by giving me a signature.
*server updates channel_partner
*I update channel_manager

3) The server tells you about the bet, and starts updating your channel by giving you new channel state and a signature.
*uses mailbox
*server updates channel_partner
*you update channel_manager

4) you finish updating your channel, and you add more money into the bet by giving the server channel state and a signature.
*you update channel_partner
*server updates channel_manager

5) The server sends you a signature for your new channel state. The bet has my money in it.
*you update channel_manager
*server updates channel_partner

6) The server starts updating my channel by sending me a signature and new channel state in my mailbox.
*uses mailbox
*server updates channel_partner
*I update channel_manager

7) I accept the channel update, and reveal my secret to the channel to update the state again. I send a signature and the new channel state to the server.
*server updates channel_manager
*I update channel_partner

8) The server updates my channel with the secret by giving me a signature.
*server updates channel_partner
*I update channel_manager

9) The server shows you the secret to starts updating your channel.
*server updates channel_partner
*you update channel_manager

10) you finish updating your channel by sending a signature to the server.
*server updates arbitrage
*server updates channel_manager
*you update channel_partner