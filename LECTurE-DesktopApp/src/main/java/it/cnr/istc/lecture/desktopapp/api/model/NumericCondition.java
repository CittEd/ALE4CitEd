/*
 * Copyright (C) 2017 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.lecture.desktopapp.api.model;

import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class NumericCondition extends Condition {

    private final NumericConditionType type;
    private final String variable;
    private final double value;

    @JsonbCreator
    public NumericCondition(@JsonbProperty("numericConditionType") NumericConditionType type, @JsonbProperty("variable") String variable, @JsonbProperty("value") double value) {
        super(ConditionType.NumericCondition);
        this.type = type;
        this.variable = variable;
        this.value = value;
    }

    public NumericConditionType getNumericConditionType() {
        return type;
    }

    public String getVariable() {
        return variable;
    }

    public double getValue() {
        return value;
    }

    public enum NumericConditionType {
        GEq, Eq, LEq
    }
}
