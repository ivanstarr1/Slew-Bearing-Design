SlewBearingDialog : dialog
{
  label = "Slew Bearing Design";
  : row {
  			: radio_column {
		  			: radio_button {
		            label = "# of Roller Pairs:";
		            key = "optNumOfRollerPairs";
		        }
		  			: radio_button {
		            key = "optBearingDesignRadius";
		            label = "Bearing Design Radius:";
		        }
		  			: radio_button {
		            key = "optBearingThickness";
		            label = "Bearing Thickness:";
		        }
		  			: radio_button {
		            key = "optInnerRadius";
		            label = "Inner Radius:";
		        }
		  			: radio_button {
		            key = "optOuterRadius";
		            label = "Outer Radius:";
		        }
		  			: radio_button {
		            key = "optRollerFilletRadius";
		            label = "Roller Fillet Radius:";
		        }
		  			: radio_button {
		            key = "optBearingGap1";
		            label = "Bearing Gap 1:";
		        }
		  			: radio_button {
		            key = "optBearingGap2";
		            label = "Bearing Gap 2:";
		        }

		  	}
		  	: column {
		  			: edit_box {
		            key = "txtNumRollerPairs";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtBearingDesignRadius";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtBearingThickness";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtInnerRadius";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtOuterRadius";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtRollerFilletRadius";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtBearingGap1";
		            label = "";
		        }
		  			: edit_box {
		            key = "txtBearingGap2";
		            label = "";
		        }
		  	}
		}
	  : row {
  			: button {
            key = "btnDraw";
            label = "Draw";
            action = "(btnDrawPressed))";
            is_default = true;
	  		}
  			: button {
            key = "btnCancel";
            label = "Cancel";
            action = "(btnCancelPressed))";
           is_cancel = true;
	  		}
		}
}