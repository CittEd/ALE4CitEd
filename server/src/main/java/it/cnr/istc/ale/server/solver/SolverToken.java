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

import it.cnr.istc.ale.api.model.EventTemplate;

/**
 *
 * @author Riccardo De Benedictis
 */
public class SolverToken {

    /**
     * This is the cause for having this token. It is the token whose expansion
     * introduced this token.
     */
    public final SolverToken cause;
    /**
     * This is the temporal variable associated to the token.
     */
    public final int tp;
    /**
     * This is the template of the token.
     */
    public final EventTemplate template;
    /**
     * An {@code Integer} representing the id of the question, if any, or
     * {@code null} if the token does not represent an answer to a question.
     */
    public final Integer question;
    boolean enabled = true;

    SolverToken(final SolverToken cause, final int tp, final EventTemplate event, Integer question) {
        this.cause = cause;
        this.tp = tp;
        this.template = event;
        this.question = question;
    }
}
