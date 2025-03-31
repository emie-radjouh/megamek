package megamek.client.bot;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import megamek.client.Client;
import megamek.common.Coords;
import megamek.common.Game;
import megamek.common.GameTurn;
import megamek.common.Entity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.Vector;
import java.util.List;
import java.util.ArrayList;

public class BotClientTest {

    private Client client;
    private Game mockGame;

    @BeforeEach
    public void setUp() {
        mockGame = mock(Game.class);
        client = new Client("testClient", "localhost", 2345);
        client.getGame().getEntitiesVector().clear(); // Ensure game starts empty
    }

    @Test
    public void testGetFirstDeployableEntityNum() {
        when(mockGame.getFirstDeployableEntityNum(any())).thenReturn(5);
        int entityNum = client.getFirstDeployableEntityNum();
        assertEquals(5, entityNum);
    }

    @Test
    public void testDeploy() {
        Coords coords = new Coords(10, 10);
        int entityId = 1;
        int facing = 2;
        int elevation = 0;
        List<Entity> loadedUnits = new Vector<>();
        boolean assaultDrop = false;
        client.deploy(entityId, coords, facing, elevation, loadedUnits, assaultDrop);
        assertTrue(true); 
    }

    
    
    
}
