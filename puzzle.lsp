

; sudo code for a* from Weiss's website
#
BestFS( node ) // A* algorithm
{
Add( node, open );
repeat
node = Best( open );
move node from open list to closed list;
if Goal( node ) then return SUCCESS;
for each child in Successors( node ) do
if child is not on open or closed lists then
Add( child, open );
else if child is on open list then
update F’( node ) and Parent( node );
else if child is on closed list then
update F’( node ) and Parent( node ) and either
a) move node from closed to open;
- OR -
b) update descendants of node on open and closed;
until Empty( open );
return FAILURE;
}
#