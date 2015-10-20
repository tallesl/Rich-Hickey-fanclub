/*
 * Created on Jan 13, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.richhickey.foil;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Rich
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class SWTHelper {
	public static void runDispatchLoop(Display display, Shell shell)
		{
		try{
			shell.open ();
			while (!shell.isDisposed ()) 
				{
				if (!display.readAndDispatch ()) display.sleep ();
				}
			}
		finally
			{
			if(!shell.isDisposed ())
				shell.dispose();
			}
		}
}
