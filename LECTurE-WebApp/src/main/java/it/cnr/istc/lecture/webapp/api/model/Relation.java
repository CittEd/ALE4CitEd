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
package it.cnr.istc.lecture.webapp.api.model;

import java.util.concurrent.TimeUnit;
import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Relation {

    private final String from;
    private final String to;
    private final Long lb;
    private final Long ub;
    private final TimeUnit unit;

    @JsonbCreator
    public Relation(@JsonbProperty("from") String from, @JsonbProperty("to") String to, @JsonbProperty("lb") Long lb, @JsonbProperty("ub") Long ub, @JsonbProperty("unit") TimeUnit unit) {
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

    public Long getLb() {
        return lb;
    }

    public Long getUb() {
        return ub;
    }

    public TimeUnit getUnit() {
        return unit;
    }
}
