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
import java.util.concurrent.TimeUnit;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class Relation {

    private final String from;
    private final String to;
    private final long lb;
    private final long ub;
    private final TimeUnit unit;

    @JsonCreator
    public Relation(@JsonProperty("from") String from, @JsonProperty("to") String to, @JsonProperty("lb") long lb, @JsonProperty("ub") long ub, @JsonProperty("unit") TimeUnit unit) {
        this.from = from;
        this.to = to;
        this.lb = lb;
        this.ub = ub;
        this.unit = unit;
    }

    public String getFrom() {
        return from;
    }

    public String getTo() {
        return to;
    }

    public long getLb() {
        return lb;
    }

    public long getUb() {
        return ub;
    }

    public TimeUnit getUnit() {
        return unit;
    }
}
