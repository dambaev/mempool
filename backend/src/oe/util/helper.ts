
export const isValidPositiveNumber = (numberString: string): boolean => {
  return parseInt(numberString) > 0;
};

export const isValidNaturalNumber = (numberString: string): boolean => {
  return parseInt(numberString) >= 0;
};

export const toBoolean = (expression: string): boolean => {
  return expression.toLowerCase() === 'true';
};

export const calculateNbdr = (span: number, toTime: number, fromTime: number): number => {
  return (span * 600 * 100) / (toTime - fromTime);
};
