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
package it.cnr.istc.lecture.api.messages;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TokenUpdate extends Message {

    public long lesson_id;
    public int id;
    public Long min, max;
    public long time;

    public TokenUpdate() {
    }

    public TokenUpdate(long lesson_id, int id, Long min, Long max, long val) {
        super(MessageType.TokenUpdate);
        this.lesson_id = lesson_id;
        this.id = id;
        this.min = min;
        this.max = max;
        this.time = val;
    }
}
