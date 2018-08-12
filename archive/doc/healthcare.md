##  Some introductory random thoughts about healthcare data

In the healthcare field today here in the US, data is increasingly in two ugly states. On the one hand it's siloed in legacy databases that are not interoperable with other systems, and on the other hand it's very leaky. Lack of coverage in the HIPAA privacy laws have enabled the data broker industry to abscond with a lot of this data and it's subsequently sold to the usual suspects, advertisers and so forth.

#### What if patients owned their data and that data was the single source of truth?

Historically the answer would be that this is impossible, how would they store it, where would they store it, how do we know it's current. We need strong centralized relational databases, HL7 transactions, feeds to our admin and billing systems, and of course now feeds to the outcome analysis databases so that we can monitor, measure and track how well our doctors are doing.

One of the unintended consequences of Obamacare (which was a great law in many ways) is the new regulations that require outcomes analysis. Too many patients were receiving poor care that required return visits to hospitals, unecessarily increasing medicare costs. The ACA law was intended to improve that. Unfortunately it's also given rise to large numbers of 3rd party vendors who provide online survey forms to capture follow ups. A patient has email before they even get home, asking to *rate your doctor*. I speculate many of these vendors make their real money by selling that data.

#### What if the single source of truth for a patient was a collection of private messages on their `SSB` feed? Something like a personal `EHR`.

Currently a typical doctor visit might go as follows. A few pesky emails in the days and weeks before, reminding the patient of the appointment. The patient shows up, signs in, and then spends time with the up front folks reviewing insurance data, coverage, etc. For new patients, or if the 7 years statute of limitations has triggered or if the patient has a new doctor, they may be asked to complete a clipboard of forms that capture patient history. This will then be entered into some legacy system, often with lots of errors. 

A nurse calls the patient in to onboard, asks for the chief complaint and takes some basic measurements, height, weight, pulse, blood pressure and so forth. Often the same repetitive questions are asked, a common question is about allergies to medication.

The doctor arrives opens his computer that runs Epic (yea sounds ike Godzilla, it is) and then curses that he doesn't have the latest from the cardiology department, or the results from the lab in the case this is an annual physical for example. Often clinical practices are located next to hospitals and there is a whole ecosystem of *systems*, each with it's own database and enterprise class software.

So at it's core, this is a real simple idea, a new approach to EHR (electronic health records). Let the patient own it, so what if it's redundant or not current, there's no reason why a patient can't have that data, on their body, phone, USB stick in their watch pocket, etc..

When it matters most and is easiest to capture is at the point of care, when the doctor is seeing the patient.

#### one nice thing about `SSB` feeds is that one can move them from one network id to another.

In the US, clinicians will almost always ask first for the patients name and birth date, and sometimes social security number for medicare patients. The reason they need both name and birth date is to increase the probability that they can uniquely identify the patient.

#### `SSB` feeds uniquely identify a node in the network based on generated key-pairs. 

This would require a mapping and an authentication step to use a feed in a clinical setting. What's nice about this is that the patient could have a lot more control over what happens to the feed once replicated. It's immutable and not as subject to human error as process of filling out forms that are later entered into some legacy enterprise system.

##  My feed is my story

#### An `SSB` feed can also capture very relevant and often missed clinical history.

`SSB` feeds can also catpure personal logs, aspects of a patients social life, and all kinds of very relevant data in terms of historical data. A good cardiologist will tell you that they can tell a good deal about a patient just by touching them. This is why doctors often begin sessions with seemingly trivial chit chat. Many chronic illnesses are very complex and the data that is most relevant just isn't captured. As wearable devices capture more and more medical data having the patient be the source of truth will be more important.
