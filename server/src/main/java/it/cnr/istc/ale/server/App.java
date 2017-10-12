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
package it.cnr.istc.ale.server;

import it.cnr.istc.ale.server.resources.UserResource;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.ws.rs.core.UriBuilder;
import org.apache.activemq.broker.Broker;
import org.apache.activemq.broker.BrokerFilter;
import org.apache.activemq.broker.BrokerPlugin;
import org.apache.activemq.broker.BrokerService;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class App {

    private static final String HOST = "localhost";
    private static final String SERVICE_PORT = "8080";
    private static final String MQTT_PORT = "1883";
    public static final EntityManagerFactory emf = Persistence.createEntityManagerFactory("ALE_PU");

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception {
        HttpServer server = GrizzlyHttpServerFactory.createHttpServer(UriBuilder.fromUri("http://" + HOST + ":" + SERVICE_PORT).build(), new ResourceConfig(UserResource.class));

        BrokerService broker = new BrokerService();
        broker.addConnector(UriBuilder.fromUri("mqtt://" + HOST + ":" + MQTT_PORT).build());
        broker.setPlugins(new BrokerPlugin[]{new BrokerPlugin() {
            @Override
            public Broker installPlugin(Broker broker) throws Exception {
                return new BrokerFilter(broker) {
                };
            }
        }});
        broker.start();
    }
}
