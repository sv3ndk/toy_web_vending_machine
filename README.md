# Naive Web Vending machine

This is just a toy project I'm using to play around with REST on Play framework as well as error handling. Most of what's described below does not exist yet.   

It contains a "vending machine" system in which a service allows remote users to buy stuff with coins and notes.  

There are 3 json/REST web services: 

- a public VendingMachine API that exposes the item buying features, though actually delegates the execution to the two other WS instead. 

- a Bank WS, basically a container of coins and notes, able to provide change

- a Stock WS, essentially a container of the sold items.

This design is on purpose a bit too convoluted for the actual needs, just as an excuse to play around with error handling while coordinating state mutations into several WS.
 
The state is just maintained as an in-memory instance for now so the thingy is neither scalable nor restartable nor anything. 

Each of these services will be "augmented" with artificially-generated failures, like sluggishness or failure to answer before or after updating the state. 

 

# Flow

The high level logic of the coordinating entry point is as follows: 

Upon receiving a purchase request: 

1. Contact the Stock WS to get the price of that item + stop there if the total money is lower than that price

2. Contact the Bank WS to deposit the payment + optionally getting some change. If successful, this updates the current set of available coins and notes in the bank

3. Get an item from the Stock WS

4. Based on result from previous step:
	- If successful: return a json status to the caller. 
	- Otherwise: send a compensating deposit() request to the bank 

A flaw in this flow is that if a failure of the coordination service itself occurs after step 2 and before 4, 
this lead to disappearing money (which is ok, we're too materialist anyhow). I guess a CQRS + an idempotent retries 
mechanism would work around that, maybe by using actors. 

# Tests and simulation

There's a request simulator that allows  to generate tons of purchase and restock requests, both via internal calls as well as WS requests.   


# Deployment and (no) monitoring

This will all be coded and deployed as part of one single Play application for now, although the calls between them will happen as if they were remotely accessed entities. We'll split that later.   

This will also be started from sbt directly for now, without packaging nor monitoring, and runtime can only be followed through log files (one separate one for each WS). We could add some visualisation of errors and performance in Kibana later. 


# Security

No security is implemented here. Some OAuth tokens logic could be included later to allow only access to the public API 