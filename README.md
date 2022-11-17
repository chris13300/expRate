# expRate
Tool to get the experience rate of an "eman-like" EXP file on an opening.<p>

Prerequisites :<br>
rename BUREAU.ini to YOUR-COMPUTER-NAME.ini<br>
set moteurEXP to path_to_your_eman_engine.exe<br>
set fichierEXP to path_to_your_experience_file.exp<p>

There are 2 ways to use this tool :<br>
- either run this command : expRate.exe path_to_your_opening_list.pgn<br>
- either run expRate.exe then enter your opening (UCI string)<p>

Normal or Reverse mode :<br>
- normal : the program starts from the opening. It searches for all the learned moves until the [80th ply](https://github.com/chris13300/expRate/blob/main/expRate/modMain.vb#L160). It caps out at 1M moves. The goal is to estimate the experience rate on an opening.
- reverse : same as normal mode but it caps out at [5k moves](https://github.com/chris13300/expRate/blob/main/expRate/modMain.vb#L243). If the [experience rate is lower than 1k](https://github.com/chris13300/expRate/blob/main/expRate/modMain.vb#L256), maybe it lacks of experience before the last move of the opening so the program restarts from the penultimate move of the opening. The goal is to find where it really begins to lack experience on an opening.
