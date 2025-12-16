# What is `LOCAL-STORAGE`?
- `LOCAL-STORAGE` is often encountered in programs that call on modules (subprograms).
- It works similar to `WORKING-STORAGE`; they are the sections that hold the variables being used for the program
- But `LOCAL-STORAGE` doesn't persist


# Why use it?
- With the way how modules work, it is important to distinguish the different use cases of `LOCAL-STORAGE` and `WORKING-STORAGE`
- `.dll` modules are essentially executables that ***must*** be called to be able to run.  Therefore, despite their special way of opening, they are still processes. 
- The main menu program becomes a parent process and the module becomes a child process. 
- Variables stored in `WORKING-STORAGE`of the modules keep their value even after going back to the main menu program (they persist in the RAM)
- This causes issues like end of file flags staying true and not resetting after running the module once
- If you use `LOCAL-STORAGE`, however, it will reset the values once the module stops running
- Whatever values the variables holds will revert to their declarations in the `LOCAL-STORAGE SECTION`

Therefore, it's to properly reset the variables and avoid causing conflicts.  If you're writing on a module, use `LOCAL-STORAGE` to reset the values of the variables each time you run it