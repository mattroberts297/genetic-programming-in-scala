# Genetic programming in scala

I hope that one day I (and engineers like me) will make ourselves obsolete. That is, we will write applications (or an application) capable of writing some (or all) other applications for us. Genetic programming (GP) is about just that: creating programs that solve problems based on expected outputs for given inputs. As a colleague of mine put it, you write the unit tests and the computer does the rest. To be clear, I think we're quite far off that, but it doesn't hurt to think big.

Most, if not all, of the algorithms implemented here are based on the excellent descriptions in the first two chapters of _A Field Guide to Genetic Programming_. I've concentrated on readability and correctness as opposed to speed of execution or reliability, so please excuse the lacklustre performance and potential for stack overflow.

