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

$(document).ready(function () {
    $.ajax({
        url: "http://localhost:8080/LECTurE-WebApp/LECTurE/users"
    }).then(function (users) {
        document.getElementById("users_count").textContent = users.length;

        var tbl = document.getElementById("users_table");
        var tbdy = document.createElement("tbody");
        for (var i = 0; i < users.length; i++) {
            var tr = document.createElement("tr");
            var td_email = document.createElement("td");
            td_email.appendChild(document.createTextNode(users[i].email));
            tr.appendChild(td_email);
            var td_first_name = document.createElement("td");
            td_first_name.appendChild(document.createTextNode(users[i].first_name));
            tr.appendChild(td_first_name);
            var td_last_name = document.createElement("td");
            td_last_name.appendChild(document.createTextNode(users[i].last_name));
            tr.appendChild(td_last_name);
            tbdy.appendChild(tr);
        }
        tbl.appendChild(tbdy);
    });
});