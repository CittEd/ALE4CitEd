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
    console.log("retrieving users..");
    $.ajax({
        url: "http://localhost:8080/LECTurE-WebApp/LECTurE/users"
    }).then(function (users) {
        console.log("found " + users.length + " users..");
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

            var td_actions = document.createElement("td");
            var actions_group = document.createElement("div");
            actions_group.className = "btn-group";
            var btn_edit = document.createElement("button");
            btn_edit.type = "button";
            btn_edit.className = "btn btn-sm";
            btn_edit.disabled = true;
            var btn_edit_icon = document.createElement("i");
            btn_edit_icon.className = "fas fa-edit";
            btn_edit.append(btn_edit_icon);
            actions_group.append(btn_edit);
            var btn_del = document.createElement("button");
            btn_del.type = "button";
            btn_del.className = "btn btn-sm";
            var btn_del_icon = document.createElement("i");
            btn_del_icon.className = "fas fa-trash";
            btn_del.append(btn_del_icon);
            var user_id = users[i].id;
            btn_del.onclick = function () {
                $.ajax({
                    url: "http://localhost:8080/LECTurE-WebApp/LECTurE/users/" + user_id,
                    type: "DELETE"
                }).then(function () {
                    tbdy.removeChild(tr);
                });
            };
            actions_group.append(btn_del);
            td_actions.append(actions_group);
            tr.appendChild(td_actions);

            tbdy.appendChild(tr);
        }
        tbl.appendChild(tbdy);
    });

    console.log("retrieving lessons..");
    $.ajax({
        url: "http://localhost:8080/LECTurE-WebApp/LECTurE/lessons"
    }).then(function (lessons) {
        console.log("found " + lessons.length + " lessons..");
        document.getElementById("lessons_count").textContent = lessons.length;

        var tbl = document.getElementById("lessons_table");
        var tbdy = document.createElement("tbody");
        for (var i = 0; i < lessons.length; i++) {
            var tr = document.createElement("tr");

            var td_name = document.createElement("td");
            td_name.appendChild(document.createTextNode(lessons[i].name));
            tr.appendChild(td_name);

            var td_state = document.createElement("td");
            td_state.appendChild(document.createTextNode(lessons[i].state));
            tr.appendChild(td_state);

            var td_time = document.createElement("td");
            td_time.appendChild(document.createTextNode(lessons[i].time));
            tr.appendChild(td_time);

            var td_actions = document.createElement("td");
            var actions_group = document.createElement("div");
            actions_group.className = "btn-group";
            var btn_edit = document.createElement("button");
            btn_edit.type = "button";
            btn_edit.className = "btn btn-sm";
            btn_edit.disabled = true;
            var btn_edit_icon = document.createElement("i");
            btn_edit_icon.className = "fas fa-edit";
            btn_edit.append(btn_edit_icon);
            actions_group.append(btn_edit);
            var btn_del = document.createElement("button");
            btn_del.type = "button";
            btn_del.className = "btn btn-sm";
            btn_del.lesson_id = lessons[i].id;
            var btn_del_icon = document.createElement("i");
            btn_del_icon.className = "fas fa-trash";
            btn_del.append(btn_del_icon);
            var lesson_id = lessons[i].id;
            btn_del.onclick = function () {
                $.ajax({
                    url: "http://localhost:8080/LECTurE-WebApp/LECTurE/lessons/" + lesson_id,
                    type: "DELETE"
                }).then(function () {
                    tbdy.removeChild(tr);
                });
            };
            actions_group.append(btn_del);
            td_actions.append(actions_group);
            tr.appendChild(td_actions);

            tbdy.appendChild(tr);
        }
        tbl.appendChild(tbdy);
    });
});