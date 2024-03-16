with Ada.Text_IO;
with Text_IO; use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Queue;

procedure Test is
    package String_Queue is new Queue(Item => Unbounded_String);
    use String_Queue;
    Q: String_Queue.Queue;
    X: Unbounded_String;
begin
    for Index in 1..16 loop
        Enqueue(Q, To_Unbounded_String(Index'Image));
    end loop;
    while Dequeue(Q, X) loop
        null;
    end loop;
    for Index in 32..42 loop
        Enqueue(Q, To_Unbounded_String(Index'Image));
    end loop;
    while Dequeue(Q, X) loop
        Put_Line(To_String(X));
    end loop;
end;
