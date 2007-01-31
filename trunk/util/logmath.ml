let logZero = neg_infinity ;;

let logOne = 0.0 ;;

let logBase = 1.0001;;
	
	
let naturalLogBase = log logBase ;;
	
let inverseNaturalLogBase = 1.0 /. naturalLogBase;;


let linearToLog linearValue = 
	if linearValue < 0.0 then
		invalid_arg ("linearToLog: param must be >= 0")
	else if linearValue = 0.0 then
		logZero 
	else
		let returnValue = (log linearValue) *. inverseNaturalLogBase in
		returnValue
		(*
			TODO:: tulcsordulas ellenorzes
			
            if (returnValue > Double.MAX_VALUE) {
                return Double.MAX_VALUE;
            } else {
                if (returnValue < -Double.MAX_VALUE) {
                    return -Double.MAX_VALUE;
                } else {
                    return (double) returnValue;
                }
            }
        }
	 *)

