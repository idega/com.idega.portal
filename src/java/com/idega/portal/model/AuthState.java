package com.idega.portal.model;

public enum AuthState {

	ONE_STEP {
		@Override
		public String toString() {
			return "ONE_STEP";
		}
	},
	SECOND_STEP_AUTH {
		@Override
		public String toString() {
			return "SECOND_STEP_AUTH";
		}
	},
	SECOND_STEP_AUTH_OVERDUE {
			@Override
			public String toString() {
				return "SECOND_STEP_AUTH_OVERDUE";
			}
	},
	SECOND_STEP_AUTH_INCORRECT {
		@Override
		public String toString() {
			return "SECOND_STEP_AUTH_INCORRECT";
		}
	};


}
