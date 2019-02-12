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

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.swing.JList;
import javax.swing.JPanel;

import com.intellij.ide.actions.OpenFileAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.ColoredListCellRenderer;
import com.intellij.ui.FileColorManager;
import com.intellij.ui.SortedListModel;
import com.intellij.ui.components.JBList;
import com.intellij.ui.components.JBScrollPane;

/**
 * @author VISTALL
 * @since 21.03.14
 */
public abstract class JavaScriptListPanel<T> extends JPanel
{
	@Nonnull
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

	public JavaScriptListPanel(@Nonnull Project project)
	{
		super(new BorderLayout());
		init();
		myProject = project;
	}

	private void init()
	{
		final JBList<T> jbList = new JBList<>(myModel);
		jbList.setCellRenderer(new ColoredListCellRenderer<T>()
		{
			@Override
			protected void customizeCellRenderer(@Nonnull JList<? extends T> jList, T t, int i, boolean b, boolean b1)
			{
				VirtualFile virtualFile = toVirtualFile(t, false);
				if(virtualFile == null)
				{
					append("<invalid>");
				}
				else
				{
					setBackground(FileColorManager.getInstance(myProject).getFileColor(virtualFile));
					append(virtualFile.getPath());
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
					T selectedValue = jbList.getSelectedValue();
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
	public abstract VirtualFile toVirtualFile(@Nonnull T value, boolean toOpen);

	public void add(T value)
	{
		myModel.add(value);
	}
}
