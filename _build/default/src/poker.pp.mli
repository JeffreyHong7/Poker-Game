Caml1999N031����            -src/poker.mli����   V  �  �  1�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����%poker��.<command-line>A@A�A@F@@��A@@�A@G@@@@�@@�������@�@@@�@@�@@@@�@@@�@�������*ocaml.text��������	u Representation of poker data

     This module represents the types and functions that a player can proceed
   with ��-src/poker.mliA@@�D p z@@@@@@���A�    �$card��F | ��F | �@@@@A@���)ocaml.docǐ������	/ the abstract type representing a playing card ��G � ��G � �@@@@@@@��F | |@@�@���A�    �&player��'I � ��(I � �@@@@A@���␠�����	) the abstract type representing a player ��5J � ��6J � �@@@@@@@��8I � �@@�@���A�    �%table��BL � ��CL �@@@@A@���7��������	< the abstract type representing the table or the game state ��PM�QMD@@@@@@@��SL � �@@�@������*PlayerSize��]OFP�^OFZ@�@�@@��bOFF@���V�������	1 raised if invalid number of players n<2 or n>10 ��oP[[�pP[�@@@@@@@'@�@���Р-create_player��yR���zR��@��@����&string���R����R��@@�@@@��@����%float���R����R��@@�@@@����&player���R����R��@@�@@@�@@@�@@@@����U�������	� [create_player n m] creates a player with name n with m money. 
    Requires: [n] must not already be the name of a current player. ���S����TK@@@@@@@���R��@�@���Р%start���VMQ��VMV@��@����$list���VM`��VMd@�����&player���VMY��VM_@@�@@@@�@@@��@����%float���VMh��VMm@@�@@@��@����%float���VMq��VMv@@�@@@����%table���VMz��VM@@�@@@�@@@�@@@�&@@@@������������
  V creates an empty table with an empty pot and no bets. The player who acts 
first is assigned by standard poker rules in which each player at the table
is assigned a card and the player with the highest card is positioned as 
button, small blind, or big blind. Raises an exception PlayerSize 
if player size is less than 2 or greater than 10 ���W����[��@@@@@@@���VMM@�@���Р,assign_cards��]���]��@��@����%table��]���]��@@�@@@����%table��]���]��@@�@@@�@@@@���Ր������	a Each player in table will receive their cards. Requires: no player has 
    their cards already ��(^���)_Ke@@@@@@@��+]��@�@���Р(pot_size��4agk�5ags@��@����%table��>agv�?ag{@@�@@@����%float��Gag�Hag�@@�@@@�@@@@���>�������8 returns table pot size ��Wb���Xb��@@@@@@@��Zagg@�@���Р$turn��cd���dd��@��@����%table��md���nd��@@�@@@����&string��vd���wd��@@�@@@�@@@@���m3�������	2 returns the name of the player that needs to act ���e����e��@@@@@@@���d��@�@���Р%raise���g����g�@��@����%table���g���g�	@@�@@@��@����%float���g���g�@@�@@@����%table���g���g�@@�@@@�@@@�@@@@����n�������	6 raises the action player's bet for the current round ���h��hW@@@@@@@���g��@�@���Р0find_next_player���jY]��jYm@��@����%table���jYp��jYu@@�@@@����&player���jYy��jY@@�@@@�@@@@@���jYY@�@���Р%check���l����l��@��@����%table���l����l��@@�@@@����%table��l���l��@@�@@@�@@@@������������	/ does nothing, moves action to the next player ��m���m��@@@@@@@��l��@�@���Р$fold��o���o��@��@����%table��'o���(o��@@�@@@����%table��0o���1o��@@�@@@�@@@@���'퐠�����	3 removes the player folding from the current round ��@p���Ap�$@@@@@@@��Co��@�@���Р$call��Lr&*�Mr&.@��@����%table��Vr&1�Wr&6@@�@@@����%table��_r&:�`r&?@@�@@@�@@@@���V�������	J matches the bet of the player whose turn it is to the table's current bet��os@@�ps@�@@@@@@@��rr&&@�@���Р*deal_cards��{u���|u��@��@����%table���u����u��@@�@@@����%table���u����u��@@�@@@�@@@@����K�������	2 deals the cards for the table, removes from deck ���v����v��@@@@@@@���u��@�@���Р)stand_off���x����x��@��@����%table���x����x��@@�@@@����%table���x���x�@@�@@@�@@@@����z�������	O determines the winner of the poker hand and their moneys adjusted accordingly ���y		��y	]@@@@@@@���x��@�@���Р,winner_names���{_c��{_o@��@����%table���{_r��{_w@@�@@@����$list���{_���{_�@�����&string���{_{��{_�@@�@@@@�@@@�@@@@������������	: returns the names of the winners with their updated money��|���|��@@@@@@@��	{__@�@���Р*show_money��~���~��@��@����&string��~���~��@@�@@@��@����%table��'~���(~��@@�@@@����%float��0~���1~��@@�@@@�@@@�@@@@���(�����	v [show_money name table] returns the amount of money that a player in table 
whose name matches the name argument has ��A���B @Bm@@@@@@@��D~��@�@���Р*show_board��M Bos�N Bo}@��@����%table��W Bo��X Bo�@@�@@@����$list��` Bo��a Bo�@�����$card��i Bo��j Bo�@@�@@@@�@@@�@@@@���a'�������	& prints the current board of the game ��z C���{ C��@@@@@@@��} Boo@�@���Р*show_cards��� E���� E��@��@����%table��� E���� E��@@�@@@����$list��� E���� E��@�����$card��� E���� E��@@�@@@@�@@@�@@@@����`�������	! prints the acting player's hand ��� F���� F�	
@@@@@@@��� E��@�@���Р*check_game��� H		�� H		@��@����%table��� H		�� H		"@@�@@@����$unit��� H		&�� H		*@@�@@@�@@@@������������	e prints the pot size, current bet, and for each player, their name, 
    current bet, and stack size ��� I	+	+�� J	s	�@@@@@@@��� H		@�@���Р2create_custom_card��� N	�	��� N	�	�@��@����#int��� N	�	��� N	�	�@@�@@@��@����$char�� N	�	�� N	�	�@@�@@@����$card�� N	�	�� N	�	�@@�@@@�@@@�@@@@���ʐ������	� creates any card where create_card with the name corresponding to the first
int and the char corresponding to the first letter of the desired suit  �� O	�	�� P
3
}@@@@@@@��  N	�	�@�@���Р4create_custom_player��) R

��* R

�@��@����&string��3 R

��4 R

�@@�@@@��@����%float��> R

��? R

�@@�@@@��@����$list��I R

��J R

�@�����$card��R R

��S R

�@@�@@@@�@@@����&player��\ R

��] R

�@@�@@@�@@@�#@@@�/@@@@���U�������	, creates a player with hand of user's choice��n S
�
��o S
�
�@@@@@@@��q R

@�@���Р3create_custom_table��z U
�
��{ U
�@��@����$list��� U
��� U
�@�����&player��� U
��� U
�@@�@@@@�@@@��@����$list��� U
�"�� U
�&@�����$card��� U
��� U
�!@@�@@@@�@@@��@����%float��� U
�*�� U
�/@@�@@@��@����&player��� U
�3�� U
�9@@�@@@����%table��� U
�=�� U
�B@@�@@@�@@@�@@@�&@@@�<@@@@������������	i creates a table with a custom list of players, custom player turn, custom
   pot size, and custom board ��� VCC�� W��@@@@@@@��� U
�
�@�@@