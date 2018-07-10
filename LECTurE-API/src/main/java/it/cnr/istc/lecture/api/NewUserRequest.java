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
package it.cnr.istc.lecture.api;

/**
 *
 * @author Riccardo De Benedictis
 */
public class NewUserRequest {

    public String email;
    public String password;
    public String first_name;
    public String last_name;
    public int concrete;
    public int president;
    public int structurer;
    public int ingenious;
    public int explorer;
    public int evaluator;
    public int worker;
    public int objectivist;

    public NewUserRequest() {
    }

    public NewUserRequest(String email, String password, String first_name, String last_name, int concrete, int president, int structurer, int ingenious, int explorer, int evaluator, int worker, int objectivist) {
        this.email = email;
        this.password = password;
        this.first_name = first_name;
        this.last_name = last_name;
        this.concrete = concrete;
        this.president = president;
        this.structurer = structurer;
        this.ingenious = ingenious;
        this.explorer = explorer;
        this.evaluator = evaluator;
        this.worker = worker;
        this.objectivist = objectivist;
    }
}
