type checksumMethod =
  | MinMax
  | Division;

module Checksum = {
  let get_minmax = row => {
    let compareReturnEither = (comparator, fst, snd) =>
      comparator(fst, snd) ? fst : snd;
    let min = row |> Js.Array.reduce(compareReturnEither((<)), max_int);
    let max = row |> Js.Array.reduce(compareReturnEither((>)), 0);
    max - min;
  };
  let get_division = row => {
    let rec findDivisor = (i1, i2, row) => {
      let isDivisible = (num1, num2) => num1 mod num2 === 0;
      let rowLength = Array.length(row);
      let num1 = row[i1];
      let num2 = row[i2];
      /* If divisible => return divisor */
      if (isDivisible(num1, num2)) {
        num1 / num2;
      } else if
        /* 2nd number is not last in row yet => move 2nd right, but dont jump to i1 position */
        (i2 < rowLength - 1) {
        let i2 = i2 + 1 == i1 ? i2 + 2 : i2 + 1;
        findDivisor(i1, i2, row);
      } else if
        /* 2nd is last, but 1st is not last yet
           => move 1st to the right and 2nd to beginning  */
        (i2 >= rowLength - 1 && i1 < rowLength - 1) {
        findDivisor(i1 + 1, 0, row);
      } else {
        0;
      };
    };
    findDivisor(0, 1, row);
  };
};

let calculateChecksum = (method, input) => {
  let splitRows = input => Js.String.split("\n", input);
  let getRow = row =>
    Js.String.split("\t", row) |> Js.Array.map(int_of_string);
  let rowChecksumFn =
    switch method {
    | MinMax => Checksum.get_minmax
    | Division => Checksum.get_division
    };
  input
  |> splitRows
  |> Js.Array.map(getRow)
  |> Js.Array.reduce((sum, row) => sum + rowChecksumFn(row), 0);
};

/* Part 1 */
assert (Day2_input.part1Test |> calculateChecksum(MinMax) == 18);

Js.log(Day2_input.actual |> calculateChecksum(MinMax) |> string_of_int);

/* Part 2 */
assert (Day2_input.part2Test |> calculateChecksum(Division) == 9);

Js.log(Day2_input.actual |> calculateChecksum(Division) |> string_of_int);