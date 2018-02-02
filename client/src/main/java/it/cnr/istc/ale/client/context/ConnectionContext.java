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
package it.cnr.istc.ale.client.context;

import it.cnr.istc.ale.api.User;
import java.util.HashMap;
import java.util.Map;
import javafx.beans.property.BooleanProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class ConnectionContext {

    private final Context ctx;
    /**
     * For each user, a boolean property representing whether the user is online
     * or not.
     */
    final Map<Long, BooleanProperty> online_users = new HashMap<>();

    ConnectionContext(Context ctx) {
        this.ctx = ctx;
    }

    public BooleanProperty isOnline(User user) {
        return online_users.get(user.getId());
    }

    public void login(String email, String password) {
        ctx.setUser(ctx.ur.login(email, password));
    }

    public void newUser(String email, String password, String first_name, String last_name) {
        ctx.setUser(ctx.ur.new_user(email, password, first_name, last_name));
    }

    public void logout() {
        ctx.setUser(null);
    }
}
