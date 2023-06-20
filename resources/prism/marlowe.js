
(function () {
  // Define Marlowe keywords
  var valueKeywords = [
    'AvailableMoney', 'Constant', 'NegValue', 'AddValue', 'SubValue', 'MulValue', 
    'DivValue', 'ChoiceValue', 'TimeIntervalStart', 'TimeIntervalEnd', 'UseValue', 'Cond'
  ].join('|');

  var observationKeywords = [
    'AndObs', 'OrObs', 'NotObs', 'ChoseSomething', 'ValueGE', 'ValueGT', 'ValueLT', 
    'ValueLE', 'ValueEQ', 'TrueObs', 'FalseObs'
  ].join('|');

  var boundKeywords = 'Bound';

  var actionKeywords = ['Deposit', 'Choice', 'Notify'].join('|');

  var payeeKeywords = ['Account', 'Party'].join('|');

  var caseKeywords = ['Case', 'MerkleizedCase'].join('|');

  var contractKeywords = ['Close', 'Pay', 'If', 'When', 'Let', 'Assert'].join('|');

  // Define Marlowe syntax
  Prism.languages.marlowe = {
    'value-keyword': {
      pattern: new RegExp('\\b(' + valueKeywords + ')\\b', 'g'),
      alias: 'value'
    },
    'observation-keyword': {
      pattern: new RegExp('\\b(' + observationKeywords + ')\\b', 'g'),
      alias: 'observation'
    },
    'bound-keyword': {
      pattern: new RegExp('\\b(' + boundKeywords + ')\\b', 'g'),
      alias: 'bound'
    },
    'action-keyword': {
      pattern: new RegExp('\\b(' + actionKeywords + ')\\b', 'g'),
      alias: 'action'
    },
    'payee-keyword': {
      pattern: new RegExp('\\b(' + payeeKeywords + ')\\b', 'g'),
      alias: 'payee'
    },
    'case-keyword': {
      pattern: new RegExp('\\b(' + caseKeywords + ')\\b', 'g'),
      alias: 'case'
    },
    'contract-keyword': {
      pattern: new RegExp('\\b(' + contractKeywords + ')\\b', 'g'),
      alias: 'contract'
    },
    'number': /\b\d+\b/g,
    'operator': /[=><]+/g,
    'punctuation': /[{}[\];(),.:]/g,
    'string': /"(?:\\.|[^\\"])*"/g
  };
}());

