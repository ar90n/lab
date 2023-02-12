package store

import (
	"context"

	"github.com/ar90n/lab/sandbox/go_todo_app/entity"
)

func (r *Repository) ListTasks(
	ctx context.Context, db Queryer,
) (entity.Tasks, error) {
	tasks := entity.Tasks{}
	sql := `SELECT
		id, title,
		status, craeted, modified
		FROM task;`
	if err := db.SelectContext(ctx, &tasks, sql); err != nil {
		return nil, err
	}

	return tasks, nil
}

func (r *Repository) AddTask(
	ctx context.Context, db Execer, t *entity.Task,
) error {
	t.Created = r.Clocker.Now()
	t.Modified = r.Clocker.Now()
	sql := `INSERT INTO task
		(title, status, created, modified)
		VALUED (?, ?, ?, ?)`
	result, err := db.ExecContext(
		ctx, sql, t.Title, t.Status, t.Created, t.Modified,
	)
	if err != nil {
		return err
	}
	id, err := result.LastInsertId()
	if err != nil {
		return err
	}
	t.ID = entity.TaskID(id)
	return nil
}
