
export const isValidPositiveNumber = (numberString: string): boolean => {
  return parseInt(numberString) > 0;
};

export const isValidNaturalNumber = (numberString: string): boolean => {
  return parseInt(numberString) >= 0;
};

export const isBoolean = (expression: string): boolean => {
  return ['true', 'false'].includes(expression);
};

export const toBoolean = (expression: string): boolean => {
  return expression.toLowerCase() === 'true';
};
