/*
 * Copyright (C) 2018 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
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
package it.cnr.istc.ale.server.solver;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.List;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TemporalNetwork {

    private final List<Double> values = new ArrayList<>();
    private final List<TemporalRelation> relations = new ArrayList<>();
    private final List<Collection<TemporalRelation>> watches = new ArrayList<>();
    private final Deque<Integer> prop_q = new ArrayDeque<>();
    private final Collection<TemporalNetworkListener> listeners = new ArrayList<>();

    /**
     * Creates and returns a new temporal variable.
     *
     * @return an {@code int} representing the temporal variable.
     */
    public int newVar() {
        int id = values.size();
        values.add(0d);
        watches.add(new ArrayList<>());
        return id;
    }

    /**
     * Gets the current value of the given temporal variable.
     *
     * @param var an {@code int} representing a temporal variable.
     * @return a {@code double} representing the value of the {@code var}
     * temporal variable.
     */
    public double getValue(final int var) {
        if (var < 0 || var >= values.size()) {
            throw new IllegalArgumentException("'var' is not a valid temporal variable");
        }
        return values.get(var);
    }

    /**
     * Sets the value of the {@code var} temporal value to {@code value} and
     * propagates the constraints, potentially updating the other variables'
     * values.
     *
     * @param var an {@code int} representing a temporal variable.
     * @param value a {@code double} representing the value of the {@code var}
     * temporal variable.
     */
    public void setValue(final int var, final double value) {
        if (var < 0 || var >= values.size()) {
            throw new IllegalArgumentException("'var' is not a valid temporal variable");
        }
        values.set(var, value);
        for (TemporalNetworkListener l : listeners) {
            l.newValue(var);
        }
        prop_q.push(var);
        propagate();
    }

    /**
     * Creates a new temporal relation between two temporal points. The temporal
     * relation represents the constraint: lb &le; end - from &le; to
     *
     * @param from an {@code int} representing a temporal variable.
     * @param to an {@code int} representing a temporal variable.
     * @param lb a {@code double} representing the lower bound of a temporal
     * relation.
     * @param ub a {@code double} representing the upper bound of a temporal
     * relation.
     */
    public void newTemporalRelation(final int from, final int to, final double lb, final double ub) {
        if (from < 0 || from >= values.size()) {
            throw new IllegalArgumentException("'from' is not a valid temporal variable");
        }
        if (to < 0 || to >= values.size()) {
            throw new IllegalArgumentException("'to' is not a valid temporal variable");
        }
        if (lb > ub) {
            throw new IllegalArgumentException("'lb' must be lower or equal than 'ub'");
        }
        relations.add(new TemporalRelation(from, to, lb, ub));
    }

    /**
     * Propagates the enforced temporal constraints.
     */
    public void propagate() {
        while (!prop_q.isEmpty()) {
            int var = prop_q.pop();
            for (TemporalRelation tr : watches.get(var)) {
                tr.propagate(var);
            }
        }
    }

    public void addTemporalNetworkListener(TemporalNetworkListener listener) {
        listeners.add(listener);
    }

    public void removeTemporalNetworkListener(TemporalNetworkListener listener) {
        listeners.remove(listener);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < values.size(); i++) {
            sb.append("tp").append(i).append(": ").append(values.get(i)).append("\n");
        }
        for (TemporalRelation tr : relations) {
            sb.append(tr);
        }
        return sb.toString();
    }

    private class TemporalRelation {

        private final int from, to;
        private final double lb, ub;

        private TemporalRelation(final int from, final int to, final double lb, final double ub) {
            this.from = from;
            this.to = to;
            this.lb = lb;
            this.ub = ub;
            watches.get(from).add(this);
            watches.get(to).add(this);
            double from_val = values.get(from), to_val = values.get(to);
            if (to_val < from_val + lb) {
                values.set(to, from_val + lb);
                for (TemporalNetworkListener l : listeners) {
                    l.newValue(to);
                }
                prop_q.push(to);
            } else if (to_val > from_val + ub) {
                values.set(to, from_val + ub);
                for (TemporalNetworkListener l : listeners) {
                    l.newValue(to);
                }
                prop_q.push(to);
            }
        }

        private void propagate(final int var) {
            assert var == from || var == to;
            double from_val = values.get(from), to_val = values.get(to);
            if (var == from) {
                if (to_val < from_val + lb) {
                    values.set(to, from_val + lb);
                    for (TemporalNetworkListener l : listeners) {
                        l.newValue(to);
                    }
                    prop_q.push(to);
                } else if (to_val > from_val + ub) {
                    values.set(to, from_val + ub);
                    for (TemporalNetworkListener l : listeners) {
                        l.newValue(to);
                    }
                    prop_q.push(to);
                }
            } else if (var == to) {
                if (from_val < to_val - ub) {
                    values.set(from, to_val - ub);
                    for (TemporalNetworkListener l : listeners) {
                        l.newValue(from);
                    }
                    prop_q.push(from);
                } else if (from_val > to_val - lb) {
                    values.set(from, to_val - lb);
                    for (TemporalNetworkListener l : listeners) {
                        l.newValue(from);
                    }
                    prop_q.push(from);
                }
            }
        }

        @Override
        public String toString() {
            return lb + " ≤ " + "tp" + to + " - " + "tp" + from + " ≤ " + ub;
        }
    }
}
