/*
 * Copyright 2013-2014 must-be.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package consulo.javascript.debugger;

import java.awt.BorderLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Comparator;

import javax.swing.JList;
import javax.swing.JPanel;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.ide.actions.OpenFileAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.FileColorManager;
import com.intellij.ui.ListCellRendererWrapper;
import com.intellij.ui.SortedListModel;
import com.intellij.ui.components.JBList;
import com.intellij.ui.components.JBScrollPane;

/**
 * @author VISTALL
 * @since 21.03.14
 */
public abstract class JavaScriptListPanel<T> extends JPanel
{
	@NotNull
	private final Project myProject;
	private SortedListModel<T> myModel = SortedListModel.create(new Comparator<T>()
	{
		@Override
		public int compare(T o1, T o2)
		{
			VirtualFile v1 = toVirtualFile(o1, false);
			VirtualFile v2 = toVirtualFile(o2, false);
			if(v1 == null || v2 == null)
			{
				return -1;
			}
			return FileUtil.comparePaths(v1.getPath(), v2.getPath());
		}
	});

	public JavaScriptListPanel(@NotNull Project project)
	{
		super(new BorderLayout());
		init();
		myProject = project;
	}

	private void init()
	{
		final JBList jbList = new JBList(myModel);
		jbList.setCellRenderer(new ListCellRendererWrapper<T>()
		{
			@Override
			public void customize(JList list, T value, int index, boolean selected, boolean hasFocus)
			{
				VirtualFile virtualFile = toVirtualFile(value, false);
				if(virtualFile == null)
				{
					setText("<invalid>");
				}
				else
				{
					setBackground(FileColorManager.getInstance(myProject).getFileColor(virtualFile));
					setText(virtualFile.getPath());
					setIcon(virtualFile.getFileType().getIcon());
				}
			}
		});
		jbList.addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent e)
			{
				if(e.getClickCount() == 2 && e.getButton() == MouseEvent.BUTTON1)
				{
					T selectedValue = (T) jbList.getSelectedValue();
					if(selectedValue == null)
					{
						return;
					}

					VirtualFile virtualFile = toVirtualFile(selectedValue, true);

					if(virtualFile != null)
					{
						OpenFileAction.openFile(virtualFile, myProject);
					}
				}
			}
		});

		add(new JBScrollPane(jbList), BorderLayout.CENTER);
	}

	@Nullable
	public abstract VirtualFile toVirtualFile(@NotNull T value, boolean toOpen);

	public void add(T value)
	{
		myModel.add(value);
	}
}
