#!/usr/local/bin/octave

W = [0.135759, 0.121925, 0.102812, 0.099937;
     0.0997992, 0.0888405, 0.076711, 0.0724143;
     0.060654, 0.0562579, 0.037116, 0.0161889;
     0.0125498, 0.00992495, 0.00575871, 0.00335193];
global Ws = cell(1, 2);
Ws{1} = W; Ws{2} = transpose(W);

global Directions = cell(1, 4);
Directions{1} = "U"; Directions{2} = "L";
Directions{3} = "D"; Directions{4} = "R";

function NewM = next (M)
  Pos = find(M == 0);
  M(Pos(randi(length(Pos)))) = randi(2) * 2;
  NewM = M;
end

function NewM = move_up (M)
  NewM = [];
  for i = 1:4
    Col = M(:, i);
    NZ = Col(Col != 0);
    LNZ = length(NZ);
    if (LNZ == 0)
      NewM = horzcat(NewM, Col);
    else
      T = zeros(4,1);
      j = 1; k = 1; copy = 0;
      while (j <= LNZ)
        if (j + 1 > LNZ)
          T(k) = NZ(j); break;
        end;
        if (!copy & (cur = NZ(j)) == (ne = NZ(j+1)))
          T(k) = cur + ne;
          j = j + 2; k++; copy = 1;
        else
          T(k) = NZ(j); j++; k++;
        end
      end
      NewM = horzcat(NewM, T);
    end
  end
end

function NewM = move (M, D)
  switch (D)
    case "U" NewM = move_up(M);
    case "D" NewM = flip(move_up(flip(M)));
    case "L" NewM = transpose(move_up(transpose(M)));
    case "R" NewM = rot90(move_up(rot90(M)), -1);
  end
end

function s = score_aux(M)
  global Ws
  T = [];
  for i = 1:2
    X = Ws{i} .* M; T(i) = sum(X(:));
  end
  s = max(T);
end

function [d, s] = score (M, Steps)
  global Directions;
  s = 0;
  total_s = 0;
  cnt = 0;
  d = Directions{1};
  if (Steps == 0)    
    for i = 1:4
      d1 = Directions{i}; M1 = move(M, d1);
      if (M1 == M && find(M1 != 0))
        s1 = 0;
      else
        s1 = score_aux(M1);
      end
      if (s1 > s)
        s = s1; d = d1;
      end
    end
    return
  else
    Pos = find(M == 0); len = length(Pos);
    if (len == 0)
      [d, s] = score(M, 0);
      return
    end
    for i = 1:len
      for j = 1:2
        M1 = M;
        M1(Pos(i)) = 2 * j;
        for k = 1:4
          d1 = Directions{k}; M2 = move(M1, d1);
          [not_used, s1] = score(M2, Steps - 1);
          if (s1 > s)
            s = s1; d = d1;
          end
        end
        total_s += s;
        cnt++;
      end
    end
    s = total_s / cnt;
  end
end

I = zeros(4, 4);
I(randperm(16, 3)) = 2
for i = 1:500
  [d, s] = score(I, 1)
  if s == 0
    break;
  end
  I1 = move(I, d);
  I = next(I1)
end
