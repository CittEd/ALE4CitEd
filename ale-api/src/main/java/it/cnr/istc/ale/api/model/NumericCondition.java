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
package it.cnr.istc.ale.api.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class NumericCondition extends Condition {

    private final ConditionType type;
    private final String variable;
    private final double value;

    @JsonCreator
    public NumericCondition(@JsonProperty("type") ConditionType type, @JsonProperty("variable") String variable, @JsonProperty("value") double value) {
        this.type = type;
        this.variable = variable;
        this.value = value;
    }

    public ConditionType getType() {
        return type;
    }

    public String getVariable() {
        return variable;
    }

    public double getValue() {
        return value;
    }

    public enum ConditionType {
        GEq, Eq, LEq
    }
}
