/*
 * Copyright (C) 2018 Riccardo De Benedictis
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
package it.cnr.istc.ale.server.time;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.List;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TemporalNetwork implements Cloneable {

    private int n_vars = 2;
    private double[] vals;
    private List<Collection<TemporalConstraint>> watches;
    private final Deque<Integer> prop_q = new ArrayDeque<>();
    private double[][] dist;
    private final Collection<TemporalConstraint> constraints = new ArrayList<>();
    private final Collection<TemporalListener> listeners = new ArrayList<>();

    public TemporalNetwork(int initialCapacity) {
        assert initialCapacity >= 2;

        // we init the variable values..
        vals = new double[initialCapacity];
        Arrays.fill(vals, Double.NaN);
        vals[0] = 0;
        vals[1] = 0;
        watches = new ArrayList<>(initialCapacity);
        watches.add(new ArrayList<>());
        watches.add(new ArrayList<>());

        // we init the distance matrix..
        dist = new double[initialCapacity][initialCapacity];
        for (int i = 0; i < dist.length; i++) {
            Arrays.fill(dist[i], Double.POSITIVE_INFINITY);
            dist[i][i] = 0;
        }

        addConstraints(new TemporalConstraint(0, 1, 0, Double.POSITIVE_INFINITY));
    }

    private TemporalNetwork(double[][] dist, int n_vars) {
        this.dist = new double[dist.length][dist.length];
        for (int i = 0; i < dist.length; i++) {
            System.arraycopy(dist[i], 0, this.dist[i], 0, dist[i].length);
        }
        this.n_vars = n_vars;
    }

    public int newTimePoint() {
        int tp = n_vars++;
        ensureCapacity(tp + 1);
        vals[tp] = 0;
        watches.add(new ArrayList<>());
        addConstraints(new TemporalConstraint(0, tp, 0, Double.POSITIVE_INFINITY), new TemporalConstraint(tp, 1, 0, Double.POSITIVE_INFINITY));
        return tp;
    }

    public final void addConstraints(TemporalConstraint... constraints) {
        this.constraints.addAll(Arrays.asList(constraints));
    }

    public boolean requiresPropagation() {
        return !constraints.isEmpty();
    }

    public boolean propagate() {
        if (constraints.size() >= n_vars * 5) {
            /**
             * Performs O(n^3) propagation through Floyd-Warshall algorithm
             */
            for (TemporalConstraint c : constraints) {
                watches.get(c.from).add(c);
                watches.get(c.to).add(c);
                dist[c.from][c.to] = Math.min(dist[c.from][c.to], c.max);
                dist[c.to][c.from] = Math.min(dist[c.to][c.from], -c.min);
            }
            for (int k = 0; k < n_vars; k++) {
                for (int i = 0; i < n_vars; i++) {
                    for (int j = 0; j < n_vars; j++) {
                        if (dist[i][k] + dist[k][j] < dist[i][j]) {
                            setWeight(i, j, dist[i][k] + dist[k][j]);
                            if (i == j && dist[i][j] < 0) {
                                return false;
                            }
                        }
                    }
                }
            }
        } else {
            /**
             * Performs IFPC propagation in O(n^2) as described in
             * "Incrementally Solving the STP by Enforcing Partial Path
             * Consistency" by Léon Planken.
             */
            for (TemporalConstraint c : constraints) {
                watches.get(c.from).add(c);
                watches.get(c.to).add(c);
                if (dist[c.to][c.from] < -c.max || dist[c.from][c.to] < c.min) {
                    return false;
                }
                if (dist[c.from][c.to] > c.max) {
                    propagate(c.from, c.to, c.max);
                }
                if (dist[c.to][c.from] > -c.min) {
                    propagate(c.to, c.from, -c.min);
                }
            }
        }
        constraints.clear();

        while (!prop_q.isEmpty()) {
            int var = prop_q.pop();
            watches.get(var).forEach(c -> c.propagate(var));
        }

        return true;
    }

    private void propagate(final int a, final int b, final double w_ab) {
        setWeight(a, b, w_ab);

        final int[] set_i = new int[n_vars];
        final int[] set_j = new int[n_vars];
        int size_i = 0, size_j = 0;

        // start with an O(n) loop
        for (int k = 0; k < n_vars; ++k) {
            if (dist[k][a] < dist[k][b] - w_ab) {
                // u -> from -> to is shorter than u -> to
                setWeight(k, b, dist[k][a] + w_ab);
                set_i[size_i++] = k;
            }
            if (dist[b][k] < dist[a][k] - w_ab) {
                // from -> to -> u is shorter than from -> u
                setWeight(a, k, w_ab + dist[b][k]);
                set_j[size_j++] = k;
            }
        }

        // finally, loop over set_i and set_j in O(n^2) time (but possibly much less)
        for (int i = 0; i < size_i; ++i) {
            for (int j = 0; j < size_j; ++j) {
                if (set_i[i] != set_j[j]) {
                    if (dist[set_i[i]][b] + dist[b][set_j[j]] < dist[set_i[i]][set_j[j]]) {
                        // set_i[i] -> from -> to -> set_j[j] is shorter than set_i[i] -> set_j[j]
                        setWeight(set_i[i], set_j[j], dist[set_i[i]][b] + dist[b][set_j[j]]);
                    }
                }
            }
        }
    }

    private void ensureCapacity(int minCapacity) {
        int capacity = dist.length;
        while (minCapacity > capacity) {
            capacity = (capacity * 3) / 2 + 1;
            if (minCapacity < capacity) {
                double[] tp_vals = new double[capacity];
                System.arraycopy(vals, 0, tp_vals, 0, vals.length);
                vals = tp_vals;

                double[][] tp_dist = new double[capacity][capacity];
                for (int i = 0; i < dist.length; i++) {
                    System.arraycopy(dist[i], 0, tp_dist[i], 0, dist[i].length);
                    Arrays.fill(tp_dist[i], dist.length, tp_dist.length, Double.POSITIVE_INFINITY);
                }
                for (int i = dist.length; i < tp_dist.length; i++) {
                    Arrays.fill(tp_dist[i], Double.POSITIVE_INFINITY);
                    tp_dist[i][i] = 0;
                }
                dist = tp_dist;
            }
        }
    }

    private void setWeight(final int from, final int to, final double weight) {
        dist[from][to] = weight;
        if (from == 0) {
            listeners.stream().forEach(l -> l.boundChange(to, -dist[to][from], dist[from][to]));
            if (vals[to] > dist[0][to]) {
                vals[to] = dist[0][to];
                prop_q.push(to);
            }
        } else if (to == 0) {
            listeners.stream().forEach(l -> l.boundChange(from, -dist[from][to], dist[to][from]));
            if (vals[from] < -dist[from][0]) {
                vals[from] = -dist[from][0];
                prop_q.push(from);
            }
        } else {
            listeners.stream().forEach(l -> l.distanceChange(from, to, -dist[to][from], dist[from][to]));
        }
    }

    public Bound distance(final int from, final int to) {
        return new Bound(-dist[to][from], dist[from][to]);
    }

    public double value(final int tp) {
        return vals[tp];
    }

    public Bound bound(final int tp) {
        return new Bound(-dist[tp][0], dist[0][tp]);
    }

    public double lb(final int tp) {
        return -dist[tp][0];
    }

    public double ub(final int tp) {
        return dist[0][tp];
    }

    @Override
    public TemporalNetwork clone() {
        return new TemporalNetwork(dist, n_vars);
    }

    @Override
    public String toString() {
        StringBuilder network = new StringBuilder();
        network.append("*****************************************************************************\n");
        for (int tp = 0; tp < n_vars; tp++) {
            network.append("tp").append(tp).append(" = ").append(domainString(tp)).append('\n');
        }
        network.append("*****************************************************************************\n");
        return network.toString();
    }

    private String domainString(int tp) {
        if (-dist[tp][0] == dist[0][tp]) {
            if (-dist[tp][0] == Double.NEGATIVE_INFINITY) {
                return "-inf";
            }
            if (-dist[tp][0] == Double.POSITIVE_INFINITY) {
                return "+inf";
            }
            return String.valueOf(-dist[tp][0]);
        }
        return "[" + (-dist[tp][0] == Double.NEGATIVE_INFINITY ? "-inf" : -dist[tp][0]) + ", " + (dist[0][tp] == Double.POSITIVE_INFINITY ? "+inf" : dist[0][tp]) + "]";
    }

    /**
     * This class represents a temporal constraint between two time points. The
     * constraint represented is:
     * <p>
     * min ≤ to - from ≤ max
     *
     * @author Riccardo De Benedictis
     */
    public final class TemporalConstraint {

        public final int from, to;
        public final double min, max;

        public TemporalConstraint(int from, int to, double min, double max) {
            assert min <= max;
            this.from = from;
            this.to = to;
            this.min = min;
            this.max = max;
        }

        private void propagate(final int var) {
            assert var == from || var == to;
            double from_val = vals[from], to_val = vals[to];
            if (var == from) {
                if (to_val < from_val + min) {
                    vals[to] = from_val + min;
                    listeners.forEach(l -> l.newValue(to, vals[to]));
                    prop_q.push(to);
                } else if (to_val > from_val + max) {
                    vals[to] = from_val + max;
                    listeners.forEach(l -> l.newValue(to, vals[to]));
                    prop_q.push(to);
                }
            } else if (var == to) {
                if (from_val < to_val - max) {
                    vals[from] = to_val - max;
                    listeners.forEach(l -> l.newValue(from, vals[from]));
                    prop_q.push(from);
                } else if (from_val > to_val - min) {
                    vals[from] = to_val - min;
                    listeners.forEach(l -> l.newValue(from, vals[from]));
                    prop_q.push(from);
                }
            }
        }

        @Override
        public String toString() {
            return min + " ≤ tp" + to + " - tp" + from + " ≤ " + max;
        }
    }

    public static final class Bound {

        public final double min;
        public final double max;

        public Bound(double min, double max) {
            this.min = min;
            this.max = max;
        }

        @Override
        public String toString() {
            return "[" + (min == Double.NEGATIVE_INFINITY ? "-inf" : min) + ", " + (max == Double.POSITIVE_INFINITY ? "+inf" : max) + "]";
        }
    }
}
