Book2Words
==========

Takes a book and splits it into words, ordered by frequency.

Right now it is painfully slow (20 minutes for Harry Potter and the Sorceror's Stone on my machine) and it doesn't recognize conjugations, plurals, or proper nouns (this is because freedict doesn't recognize them, but I am working on a solution). 

I have only tested it with Spanish, but it should work with any language that has freedict support; simply change the argumenet "spa-eng" in the writeDefinitions function to the correct dictionary for your language. 

The output file should be ready to be imported into anki except the aforementioned words groups that it does not handle (they will simply be blank on the definition side). Also, the output only gives a translation to English, no pictures or sentences, so I would think about finding those as you learn the words and inputing them from within anki. It is my vision that this functionality will eventually be done automatically by the program, so check back frequently.

Eventually, I would also like to implement a time schedule; a rough estimate of when you will be able to read the book by learning X number of words daily. 

If you would like to contribute to this project, I would be glad for your help - simply send me a pull request and I will commit your changes. Feel free to email me regarding the project at britt.mathis@gmail.com

Happy language learning!
