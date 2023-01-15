
export const isValidPositiveNumber = (numberString: string): boolean => {
  return parseInt(numberString) > 0;
};

export const isValidNeutralNumber = (numberString: string): boolean => {
  return parseInt(numberString) >= 0;
};
