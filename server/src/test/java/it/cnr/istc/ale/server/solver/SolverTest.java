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

import org.junit.Test;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class SolverTest {

    /**
     * Test of getValue method, of class Solver.
     */
    @Test
    public void testGetValue() {
        TemporalNetwork s = new TemporalNetwork();
        int tp0 = s.newVar();
        int tp1 = s.newVar();
        int tp2 = s.newVar();

        s.newTemporalRelation(tp0, tp1, 10, 20);
        s.newTemporalRelation(tp1, tp2, 10, 20);

        s.propagate();

        double tp0_val = s.getValue(tp0);
        double tp1_val = s.getValue(tp1);
        double tp2_val = s.getValue(tp2);

        s.setValue(tp0, 10);

        tp0_val = s.getValue(tp0);
        tp1_val = s.getValue(tp1);
        tp2_val = s.getValue(tp2);
    }
}
